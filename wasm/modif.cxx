function callEvalToFloatsIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    entryFunction = Module['_eval_to_floats_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);

    the_size = provideIntegers(2);

    var float_result;
    try {
        float_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
    sz = decode_size(the_size);
    if (sz < 0) {
        sz *= -1;
        Module._free(string_to_array);
        str = decode_float_as_str(float_result, sz)
        throw new Error(str);
    }
    Module._free(string_to_array);
    return decode_float(float_result, sz);
}

function callEvalToIntsIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_to_ints_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);

    the_size = provideIntegers(2);

    var int_result;

    try {
        int_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
    sz = decode_size(the_size);
    if (sz < 0) {
        sz *= -1;
        Module._free(string_to_array);
        str = decode(int_result, sz)
        throw new Error(str);
    }
    Module._free(string_to_array);
    return decode_int(int_result, sz);
}

function callEvalToStringsIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_to_strings_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);

    the_size = provideIntegers(2);

    try {

        string_result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
        sz = decode_size(the_size);
        if (sz < 0) {
            sz *= -1;
            Module._free(string_to_array);
            str = decode(string_result, sz)
            throw new Error(str);
        }
        characters = decode_int(string_result, sz);
        let str_vector = [];
        let s = "";
        for (var i = 0; i < sz; i++) {
            if (characters[i] == 0) {
                str_vector.push(s);
                s = "";
            }
            else
                s += String.fromCharCode(characters[i]);
        }
        if (s != "")
            str_vector.push(s);

        Module._free(string_to_array);
        return str_vector
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}

//Important, values should have been created with provideIntegers
function callSetqIntsIdx(lispe_idx, a_variable, vals, nb) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    if (values instanceof Int32Array == false)
        throw new Error("Expecting a Int32Array");

    entryFunction = Module['_eval_setq_ints_lispe_idx'];

    string_to_array = provideCharactersAsInts(a_variable);

    values = provideIntegers(nb, vals);

    var ret;

    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, values, nb);
    }
    catch (e) {
        Module._free(value_to_array);
        Module._free(values);
       return handleException(e);
    }

    Module._free(values);
    if (ret < 0) {
        ret *= -1;
        str = decode(string_to_array, ret)
       throw new Error(str);
    }
    Module._free(value_to_array);
    return true;
}

function callSetqFloatsIdx(lispe_idx, a_variable, vals, nb) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');

    if (values instanceof Float64Array == false)
        throw new Error("Expecting a Int32Array");


    entryFunction = Module['_eval_setq_floats_lispe_idx'];

    string_to_array = provideCharactersAsInts(a_variable);
    var ret;

    values = provideFloats(nb, vals);

    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, values, nb);
    }
    catch (e) {
        Module._free(value_to_array);
        Module._free(values);
        return handleException(e);
    }

    Module._free(values);
    if (ret < 0) {
        ret *= -1;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(value_to_array);
    return true;
}

//value is a int
function callSetqIntIdx(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_setq_int_lispe_idx'];

    string_to_array = provideCharactersAsInts(a_variable);
    var ret;

    try {
        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value);
    }
    catch (e) {
        Module._free(value_to_array);
        return handleException(e);
    }

    if (ret < 0) {
        ret *= -1;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(value_to_array);
    return true;
}

//value is float
function callSetqFloatIdx(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_setq_float_lispe_idx'];

    string_to_array = provideCharactersAsInts(a_variable);
    var ret;

    try {

        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value);
    }
    catch (e) {
        Module._free(value_to_array);
        return handleException(e);
    }

    if (ret < 0) {
        ret *= -1;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(value_to_array);
    return true;
}

//value is a string
function callSetqStringIdx(lispe_idx, a_variable, value) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    if (typeof value != "string") {
        throw new Error("Expecting a string as input");
    }

    entryFunction = Module['_eval_setq_string_lispe_idx'];

    string_to_array = provideCharactersAsInts(a_variable);
    value_to_array = provideCharactersAsInts(value);

    var ret;

    try {

        ret = entryFunction(lispe_idx, string_to_array, a_variable.length, value_to_array, value.length);
    }
    catch (e) {
        Module._free(value_to_array);
        Module._free(string_to_array);
        return handleException(e);
    }

    Module._free(value_to_array);
    if (ret < 0) {
        ret *= -1;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(string_to_array);
    return true;
}

function callEvalAsIntIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_to_int_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);
    var ret;
    try {

        ret = entryFunction(lispe_idx, string_to_array,  code.length, Math.max(20, code.length));
    }
    catch (e) {
        Module._free(string_to_array);
        return handleException(e);
    }
    if (string_to_array[0] == 27) {
        string_to_array[0] = 32;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(string_to_array);
    return ret;
}

function callEvalAsFloatIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_to_float_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);
    var ret;
    try {

        ret = entryFunction(lispe_idx, string_to_array, code.length, Math.max(20, code.length));
    }
    catch (e) {
        Module._free(string_to_array);
       return handleException(e);
    }
    if (string_to_array[0] == 27) {
        string_to_array[0] = 32;
        str = decode(string_to_array, ret)
        throw new Error(str);
    }
    Module._free(string_to_array);
    return ret;
}

function callEvalAsStringIdx(lispe_idx, code) {
    assert(runDependencies == 0, 'cannot call main when async dependencies remain! (listen on Module["onRuntimeInitialized"])');
    assert(__ATPRERUN__.length == 0, 'cannot call main when preRun functions remain to be called');


    entryFunction = Module['_eval_to_string_lispe_idx'];

    string_to_array = provideCharactersAsInts(code);

    the_size = provideIntegers(2);

    try {

        var result = entryFunction(lispe_idx, string_to_array, code.length, the_size);
        Module._free(string_to_array);
        sz = decode_size(the_size);
        if (sz < 0) {
            sz *= -1;
            str = decode(result, sz)
            throw new Error(str);    
        }
        return decode(result, sz);
    }
    catch (e) {
        Module._free(string_to_array);
        Module._free(the_size);
        return handleException(e);
    }
}
