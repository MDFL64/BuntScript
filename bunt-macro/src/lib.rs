extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::{Parse, ParseStream}, Expr, ReturnType, Token, TypeBareFn};

struct GetFunctionArgs {
    program: Expr,
    module_handle: Expr,
    func_name: Expr,
    func_ty: TypeBareFn
}

impl Parse for GetFunctionArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let program = input.parse()?;
        input.parse::<Token![,]>()?;

        let module_handle = input.parse()?;
        input.parse::<Token![,]>()?;

        let func_name = input.parse()?;
        input.parse::<Token![,]>()?;

        let func_ty: TypeBareFn = input.parse()?;

        Ok(Self{
            program,
            module_handle,
            func_name,
            func_ty
        })
    }
}

#[proc_macro]
pub fn get_function(tokens: TokenStream) -> TokenStream {
    let GetFunctionArgs {
        program,
        module_handle,
        func_name,
        func_ty
    } = syn::parse(tokens).unwrap();

    let ret_ty = match func_ty.output {
        ReturnType::Default => quote!{ Type::Void },
        ReturnType::Type(_,ty) => convert_ty(&ty)
    };

    let arg_tys = func_ty.inputs.iter().map(|arg| {
        convert_ty(&arg.ty)
    });

    quote!{
        {
            use buntscript::{Sig, Type, ToBuntType};

            let expected_sig = Sig {
                args: vec!(#(#arg_tys),*),
                result: #ret_ty
            };

            let inner = #program . get_function(#module_handle, #func_name, &expected_sig);

            let wrapper = || {
                println!("yo!");
            };

            Some(wrapper)
        }
    }.into()
}

fn convert_ty(ty: &syn::Type) -> proc_macro2::TokenStream {
    quote!{ <#ty as ToBuntType>::bunt_ty() }
}
