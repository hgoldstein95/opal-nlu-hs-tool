free-text Msg

keywords Time = "later" | "tomorrow"

alias Send = { msg: Msg, time: Time }

trait Intent = <Read>{} | <Send>Send
