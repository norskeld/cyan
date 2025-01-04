use proc_macro::TokenStream;
use proc_macro_error::{emit_error, proc_macro_error};
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DataStruct, DeriveInput, Fields, Ident, Type};

#[proc_macro_error]
#[proc_macro_derive(Located)]
pub fn located_derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let name = input.ident;
  let data = input.data;

  match data {
    | Data::Struct(data_struct) => handle_struct(name, data_struct),
    | Data::Enum(data_enum) => handle_enum(name, data_enum),
    | Data::Union(..) => {
      emit_error!(name, "Located cannot be derived for unions");

      TokenStream::new()
    },
  }
}

fn handle_struct(name: Ident, data_struct: DataStruct) -> TokenStream {
  let has_location_field = data_struct.fields.iter().any(|field| {
    field
      .ident
      .as_ref()
      .map_or(false, |ident| ident == "location")
  });

  if has_location_field {
    TokenStream::from(quote! {
      impl cyan_reporting::Located for #name {
        fn location(&self) -> &cyan_reporting::Location {
          &self.location
        }
      }
    })
  } else {
    emit_error!(name, "struct does not have a `location` field");

    TokenStream::new()
  }
}

fn handle_enum(name: Ident, data_enum: DataEnum) -> TokenStream {
  let mut match_arms = Vec::new();

  for variant in data_enum.variants.iter() {
    let variant_name = &variant.ident;

    match &variant.fields {
      // Named struct variants, e.g.
      //
      // enum Foo {
      //   Bar { value: i32, location: Location },
      // }
      | Fields::Named(fields) => {
        let has_location_field = fields.named.iter().any(|field| {
          field
            .ident
            .as_ref()
            .map_or(false, |ident| ident == "location")
        });

        if has_location_field {
          match_arms.push(quote! {
            #name::#variant_name { location, .. } => location,
          });
        } else {
          emit_error!(
            variant_name,
            "struct variant does not have a `location` field"
          );
        }
      },
      // Unnamed (tuple) variants, e.g.
      //
      // enum Foo {
      //   Bar(Baz),
      // }
      //
      // This case is hard limited to only one unnamed field, which is expected to be something
      // that implements `Located`. In theory, we could merge locations of the first and the last
      // fields. In practice, we don't need that right now.
      | Fields::Unnamed(fields) => {
        // If we have something like:
        //
        // enum Foo {
        //   Bar(Baz, Cux),
        // }
        if fields.unnamed.len() > 1 {
          emit_error!(
            variant_name,
            "tuple variant has more than one field, which is not supported"
          );

          continue;
        }

        if let Some(field) = fields.unnamed.first() {
          // Avoid producing completely broken impl.
          if matches!(&field.ty, Type::Path(..) | Type::Reference(..)) {
            match_arms.push(quote! {
              #name::#variant_name(located) => located.location(),
            });
          } else {
            emit_error!(variant_name, "tuple variant's field is not a valid type");
          }
        } else {
          emit_error!(
            variant_name,
            "tuple variant has no fields; add a single field with type deriving `Located`"
          );
        }
      },
      // Unit variants, e.g.
      //
      // enum Foo {
      //   Bar,
      // }
      //
      // These can't have a location field, so we error out.
      | Fields::Unit => {
        emit_error!(
          variant_name,
          "unit variant has no fields; change to tuple or struct variant"
        );
      },
    }
  }

  // If we didn't produce any match arms, emit error and return empty token stream.
  if match_arms.is_empty() {
    emit_error!(name, "enum has no valid variants to derive for");

    return TokenStream::new();
  }

  // If we correctly produced all match arms, emit the implementation.
  if data_enum.variants.len() == match_arms.len() {
    return TokenStream::from(quote! {
      impl cyan_reporting::Located for #name {
        fn location(&self) -> &cyan_reporting::Location {
          match self {
            #(#match_arms)*
          }
        }
      }
    });
  }

  TokenStream::new()
}
