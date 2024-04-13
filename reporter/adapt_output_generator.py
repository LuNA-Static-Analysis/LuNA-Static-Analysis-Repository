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

def get_all_cfs(cfs): # argument is an array of maps
    result = ""
    for cf in cfs:
        result += get_cf(cf)
    return result

def get_df(df): # arguments is a map
    return "Name: {name}\nDeclaration callstacks:\n{decl}\nInitialization callstacks:\n{defs}\nUse callstacks:\n{uses}".format(
        name = df["name"], decl = get_all_callstacks(df["declared"]), defs = get_all_callstacks(df["initialized"]), uses = get_all_callstacks(df["used"])
    ) + "\n"

def get_all_dfs(dfs): # argument is an array of maps
    result = ""
    for df in dfs:
        result += get_df(df)
    return result

templates_file = open("report_templates.json", "r") # open the file with templates that must be filled with details
templates_map = json.load(templates_file)

output_file = open("adapt_output.txt", "w")

json_file = open("found_errors.json", "r") # open the file with reports
error_list = json.load(json_file) # load the file to local structure (map); this must be an array
if (len(error_list) != 0):
    output_file.write("Found {error_count} errors:\n\n".format(error_count = len(error_list)))
else:
    output_file.write("No errors found.\n")

error_number = 0
for error_report in error_list:
    error_number += 1
    output_file.write("Error #" + str(error_number) + ":\n")
    error_code = error_report["error_code"][4:6] # getting an error number
    match int(error_code): # react according to what error it is exactly
        case 1:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error_report["details"]["cf"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 2:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["call_stack_entry"]["name"])
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["call_stack_entry"]))
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
        case 6:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["cf"]["name"])
                .replace("$callstack_entry", get_cf(error_report["details"]["cf"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
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
                .replace("$cfs", get_all_cfs(error_report["details"]["cfs"]))
            )
        case 12:
            output_file.write((templates_map[error_code] + "\n\n\n"))
        case 13:
            output_file.write((templates_map[error_code] + "\n")
                .replace("$dfs", get_all_dfs(error_report["details"]["dfs"]))
            )
        case 14:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$defs_callstacks", get_all_callstacks(error_report["details"]["df"]["initialized"]))
            )
        case 15: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case 16:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf_name", error_report["details"]["cfs"][0]["name"])
                .replace("$cfs", get_all_cfs(error_report["details"]["cfs"]))
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
        case 23:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$bool", str(error_report["details"]["type"]))
                .replace("$expr", str(error_report["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["where"]))
            )
        case 24:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$bool", str(error_report["details"]["type"]))
                .replace("$expr", str(error_report["details"]["condition"]))
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["where"]))
            )
        case 25:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$index", str(error_report["details"]["arg_index"]))
                .replace("$expr", str(error_report["details"]["bad_expr"]))
                .replace("$callstack_entry", get_callstack_entry(error_report["details"]["where"]))
            )
        case 26:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$expr", str(error_report["details"]["expression"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
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
        case 36:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$expr", str(error_report["details"]["expression"]))
                .replace("$callstack", get_callstack(error_report["details"]["callstack"]))
            )
        case 37: #todo
            output_file.write((templates_map[error_code] + "\n\n\n")

            )
        case _:
            print("INTERNAL ERROR: unknown error code encountered")

output_file.close()
json_file.close()
templates_file.close()
