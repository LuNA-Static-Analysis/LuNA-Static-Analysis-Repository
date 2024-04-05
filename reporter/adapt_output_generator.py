import json

def get_callstack_entry(call): # argument is a map
    return "file: {file}, line: {line}, name: {name}".format(
            file = call["file"], line = call["line"], name = call["name"]
        ) + "\n"

def get_callstack(callstack): # argument is an array of maps
    result = "\n"
    for call in callstack:
        result += get_callstack_entry(call)
    return result

def get_all_callstacks(callstacks): # argument is an array of arrays of maps
    result = ""
    for callstack in callstacks:
        result += get_callstack(callstack)
    return result

def get_cf(cf): # argument is a map
    return "name: {name}, type: {type}, file: {file}, line: {line}".format(
        name = cf["name"], type = cf["type"], file = cf["file"], line = cf["line"]
    ) + "\n"

json_file = open("found_errors.json", "r") # open the file with reports
json_map = json.load(json_file) # load the file to local structure (map)

templates_file = open("report_templates.json", "r") # open the file with templates that must be filled with details
templates_map = json.load(templates_file)

output_file = open("adapt_output.txt", "w")

error_list = json_map["error_list"] # this should be a list
output_file.write("Found {error_count} errors:\n\n".format(error_count = len(error_list)))

error_number = 0
for error_report in error_list:
    error_number += 1
    output_file.write("Error #" + str(error_number) + ":\n")
    error_code = error_report["error_code"][4:6] # getting an error number
    match int(error_code): # react according to what error it is exactly
        case 1: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")
                              
            )
        case 2:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error_report["details"]["cf"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 3:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$decl_callstacks", get_all_callstacks(error_report["details"]["df"]["declared"]))
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$defs_callstacks", get_all_callstacks(error_report["details"]["df"]["initialized"]))
            )
        case 4:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["call_stack_entry"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 5:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$decl_callstacks", get_all_callstacks(error_report["details"]["df"]["declared"]))
            )
        case 6: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 7:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$decl_callstacks", get_all_callstacks(error_report["details"]["df"]["declared"]))
            )
        case 8:
            pass #no such error exists
        case 9: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 10:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$decl_callstacks", get_all_callstacks(error_report["details"]["df"]["declared"]))
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$defs_callstacks", get_all_callstacks(error_report["details"]["df"]["initialized"]))
            )
        case 11:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 12:
            output_file.write((templates_map[error_code] + "\n\n\n"))
        case 13:#todo create new template + get dfs function
            output_file.write((templates_map[error_code] + "\n\n\n"))
        case 14:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$defs_callstacks", get_all_callstacks(error_report["details"]["df"]["initialized"]))
            )
        case 15: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 16: #todo perhaps change json format
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["cf"]["name"])
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 17:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 18: #todo
            pass
        case 19: #todo
            pass
        case 20: #todo
            pass
        case 21: #todo
            pass
        case 22: #todo
            pass
        case 23: #todo add expression string to json?
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$bool", str(error_report["details"]["type"]))
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["where"]))
            )
        case 24: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 25: #todo add expression string to json? what is arg_index?
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 26: #todo add expression string to json?
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$callstack", get_callstack(error_report["details"]["callstack"]))
            )
        case 27: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 28: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 29: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 30: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 31: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 32: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 33: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 34: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 35: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 36: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 37: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case _:
            print("INTERNAL ERROR: unknown error code encountered")

output_file.close()
json_file.close()
templates_file.close()
