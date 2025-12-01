import argv
import gleam/int
import gleam/list
import util

type Dial {
	Dial(value: Int, password: Int)
}

pub fn main() {
	case argv.load().arguments {
		["1"] -> sol1()
		["2"] -> sol2()
		_ -> panic
	}
}

fn sol1() -> Nil {
	let dial = util.slurp("input/d01.txt")
	|> list.fold(from: Dial(value: 50, password: 0), with: update1)
	echo dial.password
	Nil
}

fn update1(current: Dial, step: String) -> Dial {
	let #(sign, num) = case step {
		"L" <> x -> #(-1, x)
		"R" <> x -> #(1, x)
		_ -> panic as { "unexpected: " <> step }
	}
	let assert Ok(numval) = int.parse(num)
	let value = clamp1(current.value + sign * numval)
	Dial(value: value, password: current.password + case value {
		0 -> 1
		_ -> 0
	})
}
fn clamp1(num: Int) -> Int {
	case num {
		x if x < 0 -> clamp1(num + 100)
		x if x >= 100 -> clamp1(num - 100)
		_ -> num
	}
}

fn sol2() -> Nil {
	todo
}
