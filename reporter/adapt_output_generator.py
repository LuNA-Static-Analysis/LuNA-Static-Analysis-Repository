import json

def get_callstack(callstack): # argument is an array of maps
    result = "\n"
    for call in callstack:
        result += "file: {file}, line: {line}, name: {name}".format(
            file = call["file"], line = call["line"], name = call["name"]
        ) + "\n"
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
        case 1:
            pass
        case 2:
            output_file.write((templates_map[error_code] + "\n\n")
                #.replace("$callstack_entry", get_cf(error_report["details"]["cf"]))
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 3:
            pass
        case 4:
            pass
        case 5:
            pass
        case 6:
            pass
        case 7:
            pass
        case 8:
            pass
        case 9:
            pass
        case 10:
            pass
        case 11:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$cf", get_cf(error_report["details"]["cf"]))
            )
        case 12:
            output_file.write((templates_map[error_code] + "\n\n\n"))
        case 13:
            pass
        case 14:
            output_file.write((templates_map[error_code] + "\n\n")
                .replace("$df_name", error_report["details"]["df"]["name"])
                .replace("$uses_callstacks", get_all_callstacks(error_report["details"]["df"]["used"]))
                .replace("$defs_callstacks", get_all_callstacks(error_report["details"]["df"]["initialized"]))
            )
        case 15:
            pass
        case 16:
            pass
        case 17:
            pass
        case 18:
            pass
        case 19:
            pass
        case 20:
            pass
        case 21:
            pass
        case 22:
            pass
        case 23:
            pass
        case 24:
            pass
        case 25:
            pass
        case 26:
            pass
        case 27:
            pass
        case 28:
            pass
        case 29:
            pass
        case 30:
            pass
        case 31:
            pass
        case 32:
            pass
        case 33:
            pass
        case 34:
            pass
        case 35:
            pass
        case 36:
            pass
        case 37:
            pass
        case _:
            print("INTERNAL ERROR: unknown error code encountered")

output_file.close()
json_file.close()
templates_file.close()
