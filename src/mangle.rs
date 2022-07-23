fn encode<'a>(fields: impl AsRef<[(&'a str, &'a str)]>) -> String {
    let fields = fields.as_ref();
    let mut result = String::with_capacity(
        fields
            .iter()
            .map(|(name, value)| name.len() + value.len())
            .sum(),
    );
    result.push_str("URSL");
    for (name, value) in fields {
        result.push('_');
        result.push_str(name);
        result.push('_');
        for ch in value.chars() {
            match ch {
                '.' => result.push_str("_dot_"),
                '_' => result.push_str("__"),
                ch => result.push(ch),
            }
        }
    }
    result
}

pub fn data_label(label: &str) -> String {
    encode(&[("data", label)])
}

pub fn local_label(function: &str, label: &str) -> String {
    assert_eq!(function.chars().nth(0), Some('$'));
    encode(&[("func", &function[1..]), ("label", label)])
}

pub fn function_name(function: &str) -> String {
    assert_eq!(function.chars().nth(0), Some('$'));
    encode(&[("func", &function[1..])])
}
