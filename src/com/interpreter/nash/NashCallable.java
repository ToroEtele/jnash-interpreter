package com.interpreter.nash;

import java.util.List;

interface NashCallable {
    int arity();
    Object call(Interpreter interpreter, List<Object> arguments);
}
