import gleam/string
import simplifile

pub fn slurp(filepath: String) -> List(String) {
	let assert Ok(str) = simplifile.read(from: filepath)
	string.trim_end(str)
	|> string.split(on: "\n")
}

pub fn unwrap(val: Result(a, _)) -> a {
	let assert Ok(x) = val
	x
}
