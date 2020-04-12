use crate::object;

pub const ERR_BUILT_IN_LEN_ARG_SUPPORT: &'static str = "argument to 'len' not supported, got=";
pub const ERR_BUILT_IN_LEN_ARG_NUM: &'static str = "wrong number of arguments, got=";

pub fn builtin_len_function(arg: Vec<object::Object>) -> object::Object {
    if arg.len() != 1 {
        return object::Object::Error(format!("{}{}", ERR_BUILT_IN_LEN_ARG_NUM, arg.len()));
    }
    let arg = arg[0].clone();
    match arg {
        object::Object::String(s) => return object::Object::Integer(s.len() as i64),
        _ => {
            return object::Object::Error(format!(
                "{}{}",
                ERR_BUILT_IN_LEN_ARG_SUPPORT,
                arg.mytype()
            ))
        }
    }
}
