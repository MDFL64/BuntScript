extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{FnArg, ReturnType};

#[proc_macro_attribute]
pub fn bunt_interface(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let source: syn::ItemImpl = syn::parse(item).unwrap();

    let struct_name = source.self_ty;

    let mut wrappers = Vec::new();
    let mut fields = Vec::new();
    let mut initializers = Vec::new();

    for item in source.items {
        match item {
            syn::ImplItem::Verbatim(tokens) => {
                let func: syn::ForeignItemFn = syn::parse2(tokens).unwrap();

                let fn_name = &func.sig.ident;
                let fn_name_str = fn_name.to_string();
                let args: Vec<_> = func.sig.inputs.iter().map(|arg| {
                    if let FnArg::Typed(arg) = arg {
                        arg
                    } else {
                        panic!("bunt_interface functions should not have `self` params");
                    }
                }).collect();
                let ret_ty = &func.sig.output;

                let inner_args = args.iter().map(|arg| to_bunt_abi_ty(&arg.ty));
                let inner_ret = match ret_ty {
                    ReturnType::Default => quote!{ () },
                    ReturnType::Type(_,ty) => to_bunt_abi_ty(ty)
                };

                let wrap_arg_convert = args.iter().map(|arg| to_bunt_to_abi(&arg.pat));
                /*let wrap_ret_convert = match ret_ty {
                    ReturnType::Default => quote!{ () },
                    ReturnType::Type(_,ty) => {
                        quote!( <#ty as buntscript::FromBuntValue>::from_abi(res) )
                    }
                };*/

                let bunt_args = args.iter().map(|arg| to_bunt_ty(&arg.ty));
                let bunt_ret = match ret_ty {
                    ReturnType::Default => quote!{ buntscript::Type::Void },
                    ReturnType::Type(_,ty) => to_bunt_ty(ty)
                };

                let inner_fn = Ident::new(&format!("_raw_{}",fn_name), Span::call_site());

                fields.push(quote!{ #inner_fn : unsafe extern "C" fn( #(#inner_args),* ) -> #inner_ret });
                initializers.push(quote!{ #inner_fn : {
                    
                    let expected_sig = buntscript::Sig {
                        args: vec!(#(#bunt_args),*),
                        result: #bunt_ret
                    };
        
                    let inner_raw = program.get_function(handle, #fn_name_str, &expected_sig)?;

                    unsafe{ std::mem::transmute(inner_raw) }
                }});
                wrappers.push(quote!{
                    pub fn #fn_name (&self, state: &mut S, #(#args),*) #ret_ty {
                        // todo create (pool?) and pass execution context
                        let res = unsafe{ (self.#inner_fn)( #(#wrap_arg_convert),* ) };
                        buntscript::FromBuntValue::from_abi(res)
                    }
                });
            },
            _ => panic!("bunt_interface may only contain function signatures")
        }
    }

    let out_tokens = quote!{
        pub struct #struct_name <S> {
            _state_ty: std::marker::PhantomData<S>,
            #(#fields),*
        }
        
        impl<S> buntscript::ModuleInterface for #struct_name <S> {
            type State = S;
        
            fn new(program: &buntscript::Program<Self::State>, handle: buntscript::ModuleHandle) -> Result<Self, buntscript::CompileError> {
                Ok(Self{
                    _state_ty: std::marker::PhantomData::default(),
                    #(#initializers),*
                })
            }
        }
        
        impl<S> #struct_name <S> {
            #(#wrappers)*
        }
    }.into();

    eprintln!("{}",out_tokens);

    out_tokens
}

fn to_bunt_ty(ty: &syn::Type) -> proc_macro2::TokenStream {
    quote!{ <#ty as buntscript::ToBuntValue>::bunt_type() }
}

fn to_bunt_abi_ty(ty: &syn::Type) -> proc_macro2::TokenStream {
    quote!{ <#ty as buntscript::ToBuntValue>::AbiType }
}

fn to_bunt_to_abi(arg: &syn::Pat) -> proc_macro2::TokenStream {
    quote!{ buntscript::ToBuntValue::to_abi(& #arg) }
}
