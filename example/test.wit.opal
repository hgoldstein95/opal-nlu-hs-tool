free-text Msg

keywords Time = "later" | "tomorrow"

alias Set = { msg: Msg, time: Time }

trait Intent = <Get> {} | <Set> Set
