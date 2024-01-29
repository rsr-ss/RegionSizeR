
//const status = 'dev';
const status = 'prod';

// Current time
const now = parseInt(new Date().getTime().toString().substr(5, 5));
//console.log("Now: " + now);

const char_value_formats = new Map();
char_value_formats.set('t', 'two.sided');
char_value_formats.set('g', 'greater');
char_value_formats.set('l', 'less');
char_value_formats.set('yes', 'completely included');
char_value_formats.set('no', 'completely excluded');
char_value_formats.set('part', 'partially included');
char_value_formats.set('d', 'decrease');
char_value_formats.set('i', 'increase');
char_value_formats.set('decreasing', 'decreasing');
char_value_formats.set('increasing', 'increasing');
const num_hma_value_formats = new Map();
num_hma_value_formats.set(0, 'better');
num_hma_value_formats.set(1, 'worse');
const num_yn_value_formats = new Map();
num_yn_value_formats.set(0, 'No');
num_yn_value_formats.set(1, 'Yes');
const num_type_value_formats = new Map();
num_type_value_formats.set(1, '1. PoC only');
num_type_value_formats.set(2, '2. PoC and |max treatment effect| > delta');
num_type_value_formats.set(3, '3. PoC,|max treatment effect| > delta, and MED in dose range');
const char_mcp_parameter_formats = new Map();
char_mcp_parameter_formats.set('ED<sub>50</sub>', 'ed50');
char_mcp_parameter_formats.set('E<sub>1</sub>', 'e1');
char_mcp_parameter_formats.set('&delta;', 'delta');
char_mcp_parameter_formats.set('&delta;1', 'delta1');
char_mcp_parameter_formats.set('&delta;2', 'delta2');
char_mcp_parameter_formats.set('Hill', 'hill');

const group_formats = new Map();
group_formats.set('global_info', 'Global study');
group_formats.set('global_info_mcpmod', 'Global study');
group_formats.set('local_info', 'Subpopulation');
group_formats.set('local_info_mcpmod', 'Subpopulation');
group_formats.set('simulation_info', 'Simulation options');
group_formats.set('con_end_t_test', 'Superiority > Continuous Endpoint >> T Test');
group_formats.set('bin_end_chi_square', 'Superiority > Binary Endpoint >> Chi-square');
group_formats.set('t2e_end_life_test', 'Superiority > Time-to-event Endpoint >> Logrank Test');
group_formats.set('ni_con_end', 'Non-inferiority > Continuous Endpoint');
group_formats.set('ni_bin_end', 'Non-inferiority > Binary Endpoint');
group_formats.set('ni_t2e_end', 'Non-inferiority > Time-to-event Endpoint');
group_formats.set('mcpmod_con_end', 'MCP-Mod > Continuous Endpoint');
group_formats.set('mcpmod_bin_end', 'MCP-Mod > Binary Endpoint');

const groupn_formats = new Map();
groupn_formats.set(1, 'con_end_t_test');
groupn_formats.set(2, 'bin_end_chi_square');
groupn_formats.set(3, 't2e_end_life_test');
groupn_formats.set(4, 'ni_con_end');
groupn_formats.set(5, 'ni_bin_end');
groupn_formats.set(6, 'ni_t2e_end');
groupn_formats.set(7, 'mcpmod_con_end');
groupn_formats.set(8, 'mcpmod_bin_end');

const maxModelCount = 10;
const maxGroupCount = 10;
let conModelTypeArray = [['Linear',1], ['Emax',1], ['Sigmoidal',1], ['Log-Linear',1], ['Logistic',1], ['Exponential',1], ['Quadratic',1], ['Beta',1]];
//console.log(conModelTypeArray);
let binModelTypeArray = [['Linear',1], ['Emax',1], ['Sigmoidal',1], ['Log-Linear',1], ['Logistic',1], ['Exponential',1], ['Quadratic',1], ['Beta',1]];
//console.log(binModelTypeArray);

var sample_formula = [];
var hist_value_control, hist_file_name;
var ui_input_change_flag;
var input_error_flag = 0;
var current_local_include = new Array();
var current_pattern = new Array();
var global_doselevel_table;
var local_doselevel_table;
var con_model_table;
var bin_model_table;

const modeltype_parameter_dic = [];
modeltype_parameter_dic['Linear']      = [null             , null      , 'Linear'                                     ,'linear#NULL'              ];
modeltype_parameter_dic['Emax']        = ['ED<sub>50</sub>', null      , 'Emax: ED50 = $ed50$'                        ,'emax#ed50'                ];
modeltype_parameter_dic['Sigmoidal']   = ['ED<sub>50</sub>', 'Hill'    , 'Signoidal: ED50 = $ed50$, Hill = $h$'       ,'sigEmax#c(ed50, h)'       ];
modeltype_parameter_dic['Log-Linear']  = [null             , null      , 'log-Linear'                                 ,'linlog#NULL'              ];
modeltype_parameter_dic['Logistic']    = ['ED<sub>50</sub>', '&delta;' , 'Logistic: ED50 = $ed50$, delta = $delta$'   ,'logistic#c(ed50, delta)'  ];
modeltype_parameter_dic['Exponential'] = ['E<sub>1</sub>'  , null      , 'Exponential: E1 = $e1$'                     ,'exponential#e1'           ];
modeltype_parameter_dic['Quadratic']   = ['&delta;'        , null      , 'Quadratic: delta = $b1$'                    ,'quadratic#b1'             ];
modeltype_parameter_dic['Beta']        = ['&delta;1'       , '&delta;2', 'Beta: delta1 = $delta1$, delta2 = $delta2$' ,'betaMod#c(delta1, delta2)'];
//console.log(modeltype_parameter_dic);

const doselevel_control_default_value = '[{"id":1,"dose":"0","global_n":"60","local_n":9},{"id":2,"dose":"12.5","global_n":"60","local_n":9},{"id":3,"dose":"25","global_n":"60","local_n":9},{"id":4,"dose":"50","global_n":"60","local_n":9},{"id":5,"dose":"100","global_n":"60","local_n":9}]';
const con_end_model_default_value = '[{"column1":1,"column2":"Emax","column3":"ED<sub>50</sub>","column4":2.6,"column5":null,"column6":null},{"column1":2,"column2":"Emax","column3":"ED<sub>50</sub>","column4":12.5,"column5":null,"column6":null},{"column1":3,"column2":"Sigmoidal","column3":"ED<sub>50</sub>","column4":30.5,"column5":"Hill","column6":3.5},{"column1":4,"column2":"Quadratic","column3":"&delta;","column4":-0.00776,"column5":null,"column6":null}]';
const bin_end_model_default_value = '[{"column1":1,"column2":"Emax","column3":"ED<sub>50</sub>","column4":2.5,"column5":null,"column6":null},{"column1":2,"column2":"Emax","column3":"ED<sub>50</sub>","column4":1,"column5":null,"column6":null},{"column1":3,"column2":"Sigmoidal","column3":"ED<sub>50</sub>","column4":1,"column5":"Hill","column6":3},{"column1":4,"column2":"Sigmoidal","column3":"ED<sub>50</sub>","column4":2.5,"column5":"Hill","column6":4},{"column1":5,"column2":"Beta","column3":"&delta;1","column4":1.1,"column5":"&delta;2","column6":1.1}]';

function sum(arr) {
    var s = 0;
    for (var i=arr.length-1; i>=0; i--) {
        s += arr[i];
    }
    return s;
}

//
class Ui_Input {
    constructor (name) {
        this.name = name;
        this.values = {};
    }
    set_value (key, value) {
        this.values[key] = value;
    }
    get_value (key) {
        return this.values[key];
    }
}

class Map_Control {
    constructor () {
        this.values = [];
    }
    set_value (value) {
        this.values = value;
    }
    set_rid_value (r_id, value) {
        try {
            this.values.forEach (item => {
                if (item['r_id'] == r_id) {
                    item['r_group'].set_value(r_id, value);
                    throw new Error('Get r_id, break loop');
                } 
            });
        } catch (e) {}
    }
    get_rid_value (r_id) {
        let out = new Array();
        let value, label, ui_id, init;
        try {
            this.values.forEach (item => {
                if (item['r_id'] == r_id) {
                    value = item['r_group'].get_value(item['r_id']);
                    label = item['label'];
                    ui_id = item['ui_id'];
                    init = item['init'];
                    //console.log(label + ' ' + value);
                    throw new Error('Get value, break loop');
                } 
            });
        } catch (e) {
            out[0] = value;
            out[1] = label;
            out[2] = label + ': <font color="#ED5565"><b>' + value + '</b></font>';
            out[3] = ui_id;
            out[4] = init;
            return out;
        }
    }
    transfer_to_group (id = 'r_id', group = 'r_group') {
        const tmp_obj = {};
        this.values.forEach (item => {
            tmp_obj[item[id]] = item[group];
        });
        return new Map(Object.entries(tmp_obj));
    }
    transfer_to_output_track (destination) {
        input_error_flag = 0;
        let doselevel_error_flag = '000';
        let model_empty_flag = 0;
        let prefix = '', suffix = '';
        if (destination == 'ni_t2e_end') prefix = 'ni_';
        if (destination.substr(0, 6) == 'mcpmod') {
            suffix = '_mcpmod';
            doselevel_error_flag = doselevel_control.check_and_output();
            //console.log("Doselevel error flag:" + doselevel_error_flag);
            if (doselevel_error_flag != '000') input_error_flag = 1;
            //
            if (/con/.test(destination)) model_empty_flag = con_model_control._translate_mondel_control_value_to_map_control();
            else if (/bin/.test(destination)) model_empty_flag = bin_model_control._translate_mondel_control_value_to_map_control();
        } else {
            //
            local_info.calculate_sample_size();
            //console.log(sample_formula);
        }
        const out_obj = [];
        let sample_size = [[0, 0], [0, 0], [0, 0], [0, 1], [0, 0]];
        this.values.forEach (item => {
            let key = item['r_id'];
            let alias, ui_label_id;
            if (item['r_id_alias'] != null) alias = item['r_id_alias'];
            else alias = key;
            //
            //let label = item['label'] + ' (' + alias + ')';
            let label = item['label'];
            let dest = item['r_group'].name;
            let in_value = item['r_group'].get_value(key);
            let value_r, value_track, value_raw;
            let value_type = item['type'];
            //console.log('key: ' + key + ', value_type: ' + value_type + ', in_value:' + in_value);
            //
            if (destination == 't2e_end_life_test' || destination == 'ni_t2e_end') {
                if (key == 'totalN') {
                    for (let i = 0; i < 5; i++) {
                        sample_size[i][0] = get_sample_size(in_value, sample_formula[i]);
                    }
                } else if (key == prefix + 'enroll_len') {
                    sample_size[0][1] = parseInt(in_value);
                    sample_size[1][1] = parseInt(in_value);
                    sample_size[3][1] = sample_size[3][1] + parseInt(in_value);
                } else if (key == prefix + 'enroll_st_cn') {
                    sample_size[3][1] = sample_size[3][1] - parseInt(in_value);
                } else if (key == prefix + 'enroll_len_out') {
                    sample_size[4][1] = in_value;
                }
            }
            if (dest == destination | dest == 'simulation_info' |
                dest == 'global_info' + suffix | dest == 'local_info'+ suffix) {
                if (key.substr(0, 4) != 'raw_') {
                    value_raw = in_value;
                    if (value_type == 'boolean') {
                        value_r = 'NA';
                        value_track = in_value;
                    } else if (value_type == 'string') {
                        value_r = in_value;
                        value_track = char_value_formats.get(in_value);
                    } else if (value_type == 'number' || value_type == 'num2str') {
                        value_r = parseFloat(in_value);
                        value_track = parseFloat(in_value);
                        //
                        if (parseFloat(value_r).toString() == 'NaN') {
                            value_track = 'Error: NULL\nNumeric parameter cannot be EMPTY, please check!';
                            input_error_flag = 1;
                        }
                        //
                        if (value_type == 'num2str') {
                            if (key == 'hma') {
                                value_track = num_hma_value_formats.get(in_value);
                            } else if (/^(ni_)*(unif_|pattern)/.test(key)) {
                                value_track = num_yn_value_formats.get(in_value);
                            } else if (/^(con|bin)_type/.test(key)) {
                                value_track = num_type_value_formats.get(in_value);
                            }
                        }
                        //
                        if (key == 'bin_e0') {
                            if (parseFloat(value_r).toString() != 'NaN') {
                                if (value_r < 0 || value_r > 1) {
                                    value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the reauirement ([0, 1]), please check!';
                                    input_error_flag = 1;
                                }
                            }
                        } else if (key == 'con_emax') {
                            //decreasing(Emax<0); increasing(Emax>0);
                            let direction = map_control.get_rid_value('global_direction')[0];
                            if (parseFloat(value_r).toString() != 'NaN') {
                                if (direction == 'decreasing') {
                                    if (value_r >= 0) {
                                        value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (when decrease, Emax < 0), please check!';
                                        input_error_flag = 1;
                                    }
                                } else if (direction == 'increasing') {
                                    if (value_r <= 0) {
                                        value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (when increase, Emax > 0), please check!';
                                        input_error_flag = 1;
                                    }
                                }
                            }
                        } else if (key == 'bin_emax') {
                            //decreasing(Emax<E0); increasing(Emax>E0);
                            let direction = map_control.get_rid_value('global_direction')[0];
                            let e0 = map_control.get_rid_value('bin_e0')[0];
                            if (parseFloat(value_r).toString() != 'NaN') {
                                if (value_r < 0 || value_r > 1) {
                                    value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the reauirement ([0, 1]), please check!';
                                    input_error_flag = 1;
                                } else if (direction == 'decreasing') {
                                    if (value_r >= e0) {
                                        value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (if decrease, max response rate < placebo response rate), please check!';
                                        input_error_flag = 1;
                                    }
                                } else if (direction == 'increasing') {
                                    if (value_r <= e0) {
                                        value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (if increase, max response rate > placebo response rate), please check!';
                                        input_error_flag = 1;
                                    }
                                }
                            }
                        } else if (key == 'con_delta') {
                            let delta_yes_value = map_control.get_rid_value(key + '_yes')[0];
                            if (delta_yes_value == false) {
                                value_r = 'NA';
                            } else {
                                if (parseFloat(value_r).toString() == 'NaN') {
                                    value_track = 'Error: null\nParameter can NOT be empty, please check!';
                                    input_error_flag = 1;
                                } else {
                                    //0<delta<=|Emax|
                                    let emax = parseFloat(map_control.get_rid_value('con_emax')[0]);
                                    if (value_r != NaN && emax != NaN) {
                                        if (!(0 < value_r && value_r <= Math.abs(emax))) {
                                            value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (0<delta<=|Emax|), please check!';
                                            input_error_flag = 1;
                                        }
                                    }
                                }
                            }
                        } else if (key == 'bin_delta') {
                            //decreasing(Emax<=delta<E0); increasing(E0<delta<=Emax);
                            let delta_yes_value = map_control.get_rid_value(key + '_yes')[0];
                            let direction = map_control.get_rid_value('global_direction')[0];
                            let e0 = map_control.get_rid_value('bin_e0')[0];
                            let emax = map_control.get_rid_value('bin_emax')[0];
                            if (delta_yes_value == false) {
                                value_r = 'NA';
                            } else {
                                if (parseFloat(value_r).toString() != 'NaN') {
                                    if (value_r < 0 || value_r > 1) {
                                        value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the reauirement ([0, 1]), please check!';
                                        input_error_flag = 1;
                                    } else if (direction == 'decreasing') {
                                        if (value_r >= e0 || value_r < emax) {
                                            value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (if decrease, max response rate <= min clinical response rate < placebo response rate), please check!';
                                            input_error_flag = 1;
                                        }
                                    } else if (direction == 'increasing') {
                                        if (value_r <= e0 || value_r < emax) {
                                            value_track = 'Error: ' + value_raw + '\nParameter value does NOT meet the requirement (if increase, placebo response rate < min clinical response rate <= max response rate), please check!';
                                            input_error_flag = 1;
                                        }
                                    }                                    
                                }
                            }
                        }
                    } else if (value_type == 'object') {
                        //
                        if (typeof(in_value[0]) == 'object') value_r = 'c(' + in_value[0] + ')';
                        else value_r = in_value[0];
                        if (value_r == undefined) value_r = 'NA';
                        value_track = in_value[1];
                        //
                        value_r = based_on_dest_update_r_value (destination, key, value_r);
                        if (value_r == 'NA') value_track = 'NA';
                        if (in_value.length == 2) value_raw = in_value[1];
                        else value_raw = in_value[2];
                        //
                        if (/t2e/.test(destination)) {
                            //
                            if (/^(ni_)*recruit_/.test(key)) {
                                if (value_r != 'NA') {
                                    //console.log('Sample_size:');
                                    //console.log(sample_size);
                                    let sum_recruit = sum(value_raw);
                                    let len_recruit = value_raw.length;
                                    let expect_sample, expect_month;
                                    let error_str = '', error_str1 = '', error_str2 = '';
                                    if (/^(ni_)*recruit_global$/.test(key)) {
                                        // 0 global total
                                        expect_sample = sample_size[0][0];
                                        expect_month = sample_size[0][1];
                                        if (sum_recruit != expect_sample) {
                                            error_str1 = 'Number of "Global participants" should be ' + expect_sample + ', but customized recruitment is ' + sum_recruit + ' !\n';
                                        }
                                        if (len_recruit != expect_month) {
                                            error_str2 = 'Number of "Global study accrual time (months)" should be ' + expect_month + ', but customized recruitment is ' + len_recruit + ' !\n';
                                        }
                                    } else if (/^(ni_)*recruit_cn_in$/.test(key)) {
                                        // 3 local in global
                                        expect_sample = sample_size[3][0];
                                        expect_month = sample_size[3][1];
                                        if (sum_recruit != expect_sample) {
                                            error_str1 = 'Number of "Subpopulations within global study" should be ' + expect_sample + ', but customized recruitment is ' + sum_recruit + ' !\n';
                                        }
                                        if (len_recruit != expect_month) {
                                            error_str2 = 'Number of "Subpopulations within global study: Accrual time (months)" should be ' + expect_month + ', but customized recruitment is ' + len_recruit + ' !\n';
                                        }
                                    } else if (/^(ni_)*recruit_noncn$/.test(key)) {
                                        // 1 only global
                                        expect_sample = sample_size[1][0];
                                        expect_month = sample_size[1][1];
                                        if (sum_recruit != expect_sample) {
                                            error_str1 = 'Number of "Global participants excluding subpopulation" should be ' + expect_sample + ', but customized recruitment is ' + sum_recruit + ' !\n';
                                        }
                                        if (len_recruit != expect_month) {
                                            error_str2 = 'Number of "Global study accrual time (months)" should be ' + expect_month + ', but customized recruitment is ' + len_recruit + ' !\n';
                                        }
                                    } else if (/^(ni_)*recruit_noncn_ex$/.test(key)) {
                                        // 1 only global
                                        expect_sample = sample_size[1][0];
                                        expect_month = sample_size[1][1];
                                        if (sum_recruit != expect_sample) {
                                            error_str1 = 'Number of "Global participants" should be ' + expect_sample + ', but customized recruitment is ' + sum_recruit + ' !\n';
                                        }
                                        if (len_recruit != expect_month) {
                                            error_str2 = 'Number of "Global study accrual time (months)" should be ' + expect_month + ', but customized recruitment is ' + len_recruit + ' !\n';
                                        }
                                    } else if (/^(ni_)*recruit_cn_out$/.test(key)) {
                                        // 4 only local
                                        expect_sample = sample_size[4][0];
                                        expect_month = sample_size[4][1];
                                        if (sum_recruit != expect_sample) {
                                            error_str1 = 'Number of "Subpopulation out of global study" should be ' + expect_sample + ', but customized recruitment is ' + sum_recruit + ' !\n';
                                        }
                                        if (len_recruit != expect_month) {
                                            error_str2 = 'Number of "Subpopulation out of global study: Accrual time (months)" should be ' + expect_month + ', but customized recruitment is ' + len_recruit + ' !\n';
                                        }
                                    }
                                    error_str = error_str1 + error_str2;
                                    ui_label_id = 'ui_label_' + dest + '_' + key;
                                    if (dest == 't2e_end_life_test') ui_label_id = 'ui_label_life_test_' + key;
                                    else if (dest == 'ni_t2e_end') ui_label_id = 'ui_label_ni_t2e_end_' + key;
                                    //console.log('ui_label_id: ' + ui_label_id);
                                    if (error_str1 == '' && error_str2 == ''){
                                        $('#' + ui_label_id).css('color','black');
                                    } else {
                                        value_track = 'Error: ' + value_track + '\n' + error_str;
                                        input_error_flag = 1;
                                        $('#' + ui_label_id).css('color','#ED5565');
                                    }
                                }
                            }
                        }
                        //
                        if (/mcpmod/.test(destination)) {
                            if (key == 'global_doses') {
                                value_raw = JSON.stringify(doselevel_control.values);
                                if (doselevel_error_flag.substr(0,1) == '1') {
                                    value_track = 'Error: ' + value_track + '\nInput NOT correct, please check!';
                                }
                            } else if (key == 'global_n_arm') {
                                if (doselevel_error_flag.substr(1,1) == '1') {
                                    value_track = 'Error: ' + value_track + '\nInput NOT correct, please check!';
                                }
                            } else if (key == 'local_n_arm_reg') {
                                if (doselevel_error_flag.substr(2,1) == '1') {
                                    value_track = 'Error: ' + value_track + '\nInput NOT correct, please check!';
                                }
                            } else if (/_model1/.test(key)) {
                                if (/con/.test(key)) value_raw = JSON.stringify(con_model_control.values);
                                else if (/bin/.test(key)) value_raw = JSON.stringify(bin_model_control.values);
                            }
                        }
                    }
                    //
                    value_r = based_on_dest_update_r_value (destination, key, value_r);
                    //if (value_r == 'NA') value_track = 'NA';
                    //console.log(key + ': ' + value_type + ' - ' + in_value + ' : ' + value_r + ' <- ' + value_track + ' <- ', value_raw);
                    out_obj.push([key, alias, dest, group_formats.get(dest) , label, value_type, value_r, value_track, value_raw]);
                }
            }
        });
        //console.log('transfer_to_output_track()');
        //console.log(out_obj);
        return out_obj;
    }
    transfer_to_label (id = 'r_id', description = 'label') {
        const tmp_obj = {};
        this.values.forEach (item => {
            tmp_obj[item[id]] = item[description];
        });
        return new Map(Object.entries(tmp_obj));
    }
}

class Doselevel_Control {
    constructor () {
        this.values = [];
        this.global_totaln = 0;
        this.local_totaln = 0;
    }
    //
    set_values_from_history_arrstr_to_doselevel_control (value_arrstr) {
        this.values = JSON.parse(value_arrstr);
    }
    get_values_from_doselevel_control_to_global_table () {
        let return_arr = this.values.map((item, index) => {
            const container = {};
            container.column1 = index + 1; //
            container.column2 = item['dose'];
            container.column3 = item['global_n'];
            return container;
        });
        return return_arr;
    }
    get_values_from_doselevel_control_to_local_table () {
        let return_arr = this.values.map((item, index) => {
            const container = {};
            container.column1 = index + 1; //
            container.column2 = item['global_n'];
            container.column3 = item['local_n'];
            return container;
        });
        return return_arr;
    }
    //
    set_value_from_global_table (global_table) {
        //console.log('set_value_from_global_table:');
        let sum = 0, pct = 0;
        global_table.map((item, index) => {
            //console.log('index: ' + index);
            //console.log(item);
            if (this.values.at(index) == undefined) this.values[index] = {};
            this.values[index].id = item.column1;
            this.values[index].dose = item.column2;
            if (item.column3 == null || item.column3 == '') {
                this.values[index].global_n = null;
            } else {
                this.values[index].global_n = item.column3;
                sum += parseFloat(item.column3);
            }
        });
        this.global_totaln = sum;
        map_control.set_rid_value('global_totaln', sum);
        pct = (this.local_totaln / sum * 100).toFixed(2);
        map_control.set_rid_value('local_pct', pct);
        //
        $('#ui_number_mcpmod_global_totaln').val(sum);
        $('#ui_number_mcpmod_local_pct').val(pct);
    }
    //
    remove_row (index) {
        this.values.splice(index, 1);
        var sum1 = 0, sum2 = 0, pct = 0;
        this.values.map((item) => {
            if (parseFloat(item.global_n).toString() != 'NaN') sum1 += parseFloat(item.global_n);
            if (parseFloat(item.local_n).toString() != 'NaN') sum2 += parseFloat(item.local_n);
        });
        this.global_totaln = sum1;
        map_control.set_rid_value('global_totaln', sum1);
        this.local_totaln = sum2;
        map_control.set_rid_value('local_totaln', sum2);
        pct = (sum2 / sum1 * 100).toFixed(2);
        map_control.set_rid_value('local_pct', pct);
        //const id_group_ref = r_id_groups.get('totaln_mcpmod');
        //id_group_ref.set_value('totaln_mcpmod', (this.local_totaln / sum * 100).toFixed(2));
        //
        $('#ui_number_mcpmod_global_totaln').val(sum1);
        $('#ui_number_mcpmod_local_totaln').val(sum2);
        $('#ui_number_mcpmod_local_pct').val(pct);
    }
    //
    set_value_from_local_table (local_table) {
        //console.log('set_value_from_local_table:');
        let sum = 0, pct = 0;
        local_table.map(item => {
            //console.log('colomn3: ' + item.column3);
            if (parseFloat(item.column3).toString() != 'NaN') {
                this.values[item.column1 - 1]['local_n'] = parseFloat(item.column3);
                sum += parseFloat(item.column3);
            }
        });
        this.local_totaln = sum;
        map_control.set_rid_value('local_totaln', sum);
        if (sum > 0) {
            pct = (sum / this.global_totaln * 100).toFixed(2);
        }
        map_control.set_rid_value('local_pct', pct);
        //
        $('#ui_number_mcpmod_local_totaln').val(sum);
        $('#ui_number_mcpmod_local_pct').val(pct);
    }
    //
    get_value_for_local_table () {
        //console.log('get_value_for_local_table:');
        const temp = this.values.map(item => {
            //console.log(item);
            const container = {};
            container.column1 = item.id;
            container.column2 = (isNaN(item.global_n)) ? null : item.global_n;
            if (item.local_n == undefined) container.column3 = '';
            else container.column3 = item.local_n;
            return container;
        });
        return temp;
    }
    //
    push_data_to_map_control () {
        //console.log('Doselevel_Control - push_data_to_map_control():');
        const tmp_arr = [[], [], []];
        this.values.forEach ((item, index) => {
            tmp_arr[0][index]=parseFloat(item.dose);
            tmp_arr[1][index]=parseFloat(item.global_n);
            tmp_arr[2][index]=parseFloat(item.local_n);
        });
        map_control.set_rid_value('global_doses', ['c(' + tmp_arr[0].join(', ') + ')', tmp_arr[0].join(', ')]);
        map_control.set_rid_value('global_n_arm', ['c(' + tmp_arr[1].join(', ') + ')', tmp_arr[1].join(', ')]);
        map_control.set_rid_value('local_n_arm_reg', ['c(' + tmp_arr[2].join(', ') + ')', tmp_arr[2].join(', ')]);
    }
    //
    check_and_output () {
        //console.log('Doselevel_Control - check_and_output():');
        //
        let dose_flag = 0;
        let global_flag = 0;
        let local_flag = 0;
        //
        global_doselevel_table.clearHighlightedCells();
        local_doselevel_table.clearHighlightedCells();
        //
        this.values.map((item, index) => {
            //console.log('Index: ' + index);
            //console.log(item.global_n + ", " + item.local_n);
            // 
            if (item.dose == null) {
                global_doselevel_table.highlightCell(index + 1, 1);
                //console.log('Error: Dosage in group is NULL');
                dose_flag = 1;
            } else if (parseFloat(item.dose).toString() == 'NaN') {
                global_doselevel_table.highlightCell(index + 1, 1);
                //console.log('Error: Dosage in group is String');
                dose_flag = 1;
            }
            //
            if (item.global_n == null) {
                global_doselevel_table.highlightCell(index + 1, 2);
                //console.log('Error: Global Total is NULL');
                global_flag = 1;
            } else if (parseInt(item.global_n, 10) != item.global_n) {
                global_doselevel_table.highlightCell(index + 1, 2);
                //console.log('Error: Global Total is NOT Int');
                global_flag = 1;
            }
            if (item.local_n == null) {
                local_doselevel_table.highlightCell(index + 1, 2);
                //console.log('Error: Local Total is NULL');
                local_flag = 1;
            } else if (parseInt(item.local_n,10) != item.local_n) {
                local_doselevel_table.highlightCell(index + 1, 2);
                //console.log('Error: Local Total is NOT Int');
                local_flag = 1;
            } 
            if (parseFloat(item.local_n) > parseFloat(item.global_n)) {
                //console.log('Error: Inedex = ' + index + ', Global Total = ' + item.global_n + ', Local Total = ' + item.local_n);
                local_doselevel_table.highlightCell(index + 1, 2);
                local_flag = 1;
            }
        });
        return ''.concat(dose_flag, global_flag, local_flag);
    }
}

class McpMod_Model_Control {
    constructor () {
        this.values = [];
        this.length = 0;
        this.prefix = '';
    }
    //
    set_values_from_history_arrstr_to_model_control (value_arrstr) {
        this.values = JSON.parse(value_arrstr);
        this.length = this.values.length;
        //
        for (let i = 0; i < maxModelCount; i++) {
            if (i < this.length) {
                map_control.set_rid_value(this.prefix + '_model' + eval(i + 1), this.values[i]);
            } else {
                map_control.set_rid_value(this.prefix + '_model' + eval(i + 1), ['NA', 'NA', 'NA']);
            }
        }
    }
    //
    transfer_label_to_parameter (label) {
        let out = char_mcp_parameter_formats.get(label);
        //console.log("Con_Model_Control.transfer_label_to_parameter(): " + label + ' -> ' + out);
        return out;
    }
    //
    _set_value_from_model_table_to_control (item_objarr) {
        //console.log(this.prefix + '_Model_Control._set_value_from_model_table_to_control(): start');
        //console.log('# Input:');
        //console.log(item_objarr);
        //
        this.values = item_objarr.map((item, index) => {
                const container = {};
                container.column1 = parseInt(item.column1);
                container.column2 = item.column2;
                container.column3 = item.column3;
                container.column4 = parseFloat(item.column4);
                container.column5 = item.column5;
                container.column6 = parseFloat(item.column6);
                return container;
            });
        this.length = item_objarr.length;
        //console.log('# output:');
        //console.log(this.values);
        //console.log(this.prefix + '_Model_Control._set_value_from_model_table_to_control(): end');
    }
    //
    get_value_from_model_table_compare_update_control (table_objarr) {
        //console.log(this.prefix + '_Model_Control.get_value_from_model_table_compare_update_control(): start');
        let control_objarr = new Array();
        let out_objarr = new Array();
        //
        control_objarr = this.values;
        let control_length = control_objarr.length;
        //
        //
        let control_obj_item;
        out_objarr = table_objarr.map((table_obj_item, index) => {
            //console.log('# Index: ' + index);
            //console.log('# table_obj:');
            //console.log(table_obj_item);
            if (index >= control_length) {
                control_obj_item = {column1:1, column2:'', column3:'', column4:null, column5:'', column6:null};
            } else {
                control_obj_item = control_objarr[index];
            }
            //console.log('# control_obj:');
            //console.log(control_obj_item);
            if (table_obj_item.column2 != control_obj_item.column2) {
                //console.log('# Compare modelType: ' + control_obj_item.column2 + ' -> ' + table_obj_item.column2);
                return this._base_type_assign_parameter_default_label_and_value (table_obj_item);
            } else {
                return table_obj_item;
            }
        });
        //
        this._set_value_from_model_table_to_control(out_objarr);
        //
        //console.log('# return:')
        //console.log(out_objarr);
        //console.log('Con_Model_Control.get_value_from_model_table_compare_update_control(): end');
        return out_objarr;
    }
    //
    _base_type_assign_parameter_default_label_and_value (item_obj) {
        //console.log(this.prefix + '_Model_Control.base_type_assign_parameter_default_value(): start');
        //console.log('# Input:');
        //console.log(item_obj);
        const l1 = modeltype_parameter_dic[item_obj.column2][0];
        const l2 = modeltype_parameter_dic[item_obj.column2][1];
        const v1 = (l1 === null) ? null : 0;
        const v2 = (l2 === null) ? null : 0;
        const out_obj = {column1:item_obj.column1, column2:item_obj.column2,
                         column3:l1, column4:v1, column5:l2, column6:v2};
        //console.log('# Output:');
        //console.log(out_obj);
        //console.log(this.prefix + '_Model_Control.base_type_assign_parameter_default_value(): end');
        return out_obj;
    }
    //
    remove_row (index) {
        this.values.splice(index, 1);
    }
    //
    _translate_mondel_control_value_to_map_control () {
        //console.log(this.prefix + '_Model_Control._translate_mondel_control_value_to_map_control(): start');
        let out_model_objarr, model_empty_flag = 0;
        let model_repeat_obj = {linear: 0, emax:0, sigEmax:0, logistic:0, exponential:0, quadratic:0, betaMod:0};
        //
        if (this.length == 0) {
            model_empty_flag = 1;
            out_model_objarr = new Array(['???Empty Model???', 'Error: NaN\nModels are NOT corrent, please check!', 'NaN']);
        } else {
            //
            out_model_objarr = this.values.map((model_item, index) => {
                //console.log('# Index: ' + index);
                //console.log('# model_item:');
                //console.log(model_item);
                let out_str;
                let parameters_obj = {Id:null, b1:null, b2:null, delta:null, delta1:null, delta2:null, e0:null, e1:null, ed50:null, eMax:null, h:null};
                parameters_obj.e0 = map_control.get_rid_value(this.prefix + '_e0')[0];
                parameters_obj.eMax = map_control.get_rid_value(this.prefix + '_emax')[0];
                parameters_obj.delta = map_control.get_rid_value(this.prefix + '_delta')[0];
                let model = model_item.column2;
                let out_pattern_track = '', out_pattern_r = '';
                if (model == '') {
                    out_pattern_track = 'Error: NaN\nModel type can NOT be enpty, please check!';
                    out_pattern_r = '???Empty Model???';
                } else {
                    out_pattern_track = modeltype_parameter_dic[model][2];
                    out_pattern_r = modeltype_parameter_dic[model][3];
                    //
                    if (model == 'Linear') {
                        model_repeat_obj.linear+=1;
                        parameters_obj.Id = model_repeat_obj.linear;
                    } else if (model == 'Emax') {
                        model_repeat_obj.emax+=1;
                        parameters_obj.Id = model_repeat_obj.emax;
                        parameters_obj.ed50 = model_item.column4;
                    } else if (model == 'Sigmoidal') {
                        model_repeat_obj.sigEmax+=1;
                        parameters_obj.Id = model_repeat_obj.sigEmax;
                        parameters_obj.ed50 = model_item.column4;
                        parameters_obj.h = model_item.column6;
                    } else if (model == 'Log-Linear') {
                        model_repeat_obj.linlog+=1;
                        parameters_obj.modelId = model_repeat_obj.linlog;
                    } else if (model == 'Logistic') {
                        model_repeat_obj.logistic+=1;
                        parameters_obj.Id = model_repeat_obj.logistic;
                        parameters_obj.ed50 = model_item.column4;
                        parameters_obj.delta = model_item.column6;
                    } else if (model == 'Exponential') {
                        model_repeat_obj.exponential+=1;
                        parameters_obj.Id = model_repeat_obj.exponential;
                        parameters_obj.e1 = model_item.column4;
                    } else if (model == 'Quadratic') {
                        model_repeat_obj.quadratic+=1;
                        parameters_obj.Id = model_repeat_obj.quadratic;
                        parameters_obj.b1 = model_item.column4;
                    } else if (model == 'Beta') {
                        model_repeat_obj.betaMod+=1;
                        parameters_obj.Id = model_repeat_obj.betaMod;
                        parameters_obj.delta1 = model_item.column4;
                        parameters_obj.delta2 = model_item.column6;
                    }
                    //console.log('# parameters_obj:');
                    //console.log(parameters_obj);
                    //
                    for (const [key, value] of Object.entries(parameters_obj)) {
                        out_pattern_track = out_pattern_track.replace('$'+ key + '$', value);
                        out_pattern_r = out_pattern_r.replace(key, value);
                    }
                    //
                    if (/NaN/.test(out_pattern_track)) out_pattern_track = 'Error:' + out_pattern_track + '\nParameter value NOT corrent, please check!';
                    //console.log(out_pattern_track);
                    //console.log(out_pattern_r);
                }
                return [out_pattern_r, out_pattern_track, 'NA'];
            });
        }
        //
        let check_model_arrobj = new Object();
        for (let i = 0; i < out_model_objarr.length; i ++) {
            //console.log(i);
            //console.log(out_model_objarr[i][0]);
            let model = out_model_objarr[i][0].split('#');
            //console.log(model[0] + ', ' + model[1]);
            if (model[0] in check_model_arrobj == false) {
                check_model_arrobj[model[0]] = new Array(out_model_objarr.length);
            }
            check_model_arrobj[model[0]][i] = model[1];
        }
        //console.log(check_model_arrobj);
        //
        for (let objItem of Object.entries(check_model_arrobj)) {
            let model = objItem[0];
            let valueArr = objItem[1];
            //console.log(objItem);
            let selectFlag, selectCount = 0, errorFlag = '';
            for (let i = 0; i < valueArr.length; i++) {
                if (valueArr[i] != undefined) {
                    selectFlag = selectFlag + '1';
                    selectCount+=1;
                } else selectFlag = selectFlag + '0';
            }
            //
            if (/10+1/.test(selectFlag)) {
                //
                errorFlag = '1';
            } else errorFlag = '0';
            //
            if (selectCount > 1) {
                var notnull_valueArr = valueArr.filter((value) => {
                    return value && value.trim();
                });
                if (Array.from(new Set(notnull_valueArr)).length != notnull_valueArr.length) {
                    //
                    errorFlag = errorFlag + '1';
                } else {
                    errorFlag = errorFlag + '0';
                }
            } else errorFlag = errorFlag + '0';
            if (/1/.test(errorFlag)) {
                let errorStr = '';
                if (errorFlag.substr(0, 1) == '1') errorStr = errorStr + '\nPlease ensure that models of the same type are neighboring each other (i.e., an Emax model should be sequenced with another Emax model).';
                if (errorFlag.substr(1, 1) != '0') errorStr = errorStr + '\nDuplicate models, please check!';
                //console.log('# ErrorFlag: ' + errorStr);
                for (let i = 0; i < out_model_objarr.length; i++) {
                    let model_name = out_model_objarr[i][0].split('#')[0];
                    let model_value_track = out_model_objarr[i][1];
                    if (model_name == model) {
                        if (/^Error:/.test(model_value_track)) out_model_objarr[i][1] = model_value_track + errorStr;
                        else out_model_objarr[i][1] = 'Error: ' + model_value_track + errorStr;
                    }
                }
            }
        }
        //console.log('# output:');
        //console.log(out_model_objarr);
        //
        for (let i = 0; i < maxModelCount; i++) {
            if (i < out_model_objarr.length) {
                map_control.set_rid_value(this.prefix + '_model' + eval(i + 1), out_model_objarr[i]);
            } else {
                map_control.set_rid_value(this.prefix + '_model' + eval(i + 1), ['NA', 'NA', 'NA']);
            }
        }
        //
        let mods_str = 'models <- Mods(\n';
        let id = 0, nextline = '\t  ';
        for (let objItem of Object.entries(check_model_arrobj)) {
            if (id > 0) nextline = '\t, ';
            id+=1;
            let model = objItem[0];
            let valueArr = objItem[1];
            //console.log('# ' + model + ': ' + valueArr);
            valueArr = valueArr.filter(function (value) {
                return value && value.trim(); 
            });
            if (valueArr.length > 1) {
                if (model == 'sigEmax' || model == 'linlog') {
                    mods_str = mods_str + nextline + model + ' = rbind(' + valueArr.join(', ') + ')\n';
                } else {
                    mods_str = mods_str + nextline + model + ' = c(' + valueArr.join(', ') + ')\n';
                }
            } else {
                mods_str = mods_str + nextline + model + ' = ' + valueArr[0] + '\n';
            }
        }
        //
        let direction_value = map_control.get_rid_value('global_direction')[0];
        mods_str = mods_str  + '\t, direction = "' + direction_value + '"\n';
        //
        mods_str = mods_str + '\t, doses = ' + map_control.get_rid_value('global_doses')[0][0] + '\n';
        //
        if (this.prefix != 'bin') {
            mods_str = mods_str + '\t, placEff = ' + map_control.get_rid_value(this.prefix + '_e0')[0] + '\n';
        } else {
            mods_str = mods_str + '\t, placEff = logit(' + map_control.get_rid_value(this.prefix + '_e0')[0] + ')\n';
        }
        //
        if (this.prefix != 'bin') {
            mods_str = mods_str + '\t, maxEff = ' + map_control.get_rid_value(this.prefix + '_emax')[0] + '\n';
        } else {
            mods_str = mods_str + '\t, maxEff = logit(' + map_control.get_rid_value(this.prefix + '_emax')[0] + ') - logit(' + map_control.get_rid_value(this.prefix + '_e0')[0] + ')\n';
        }
        //
        mods_str = mods_str + '\t)';
        map_control.set_rid_value(this.prefix + '_mods_str', [mods_str, 'NA','NA']);
        //console.log('# return: model_empty_flag = ' + model_empty_flag);
        //console.log(this.prefix + '_Model_Control._translate_mondel_control_value_to_map_control(): end');
        return model_empty_flag;
    }
}

class Con_Model_Control extends McpMod_Model_Control {
    constructor() {
        super();
        this.prefix = 'con';
    }
}

class Bin_Model_Control extends McpMod_Model_Control {
    constructor() {
        super();
        this.prefix = 'bin';
    }
}

function based_on_dest_update_r_value (dest, r_id_name, value) {
    const local_include = current_local_include[0];
    const pattern = current_pattern[0];
    let out;
    out = value;
    //
    if ((dest == 't2e_end_life_test' | dest == 'ni_t2e_end') && r_id_name == 'chinese_all_in') {
        if (value == 'yes') out = 1;
        else if (value == 'part') out = 2;
        else if (value == 'no') out = 0;
    }
    //
    if ((dest.substr(0, 3) != 'ni_') && (r_id_name == 'nim' | r_id_name == 'hma')) {
        out = 'NA';
    }
    //
    if (dest == 'bin_end_chi_square' && r_id_name == 'altHypo') out = 'NA';
    //
    if (dest == 'ni_bin_end' && r_id_name == 'altHypo') out = 'NA';
    //
    //
    if (dest == 't2e_end_life_test' | dest == 'ni_t2e_end') {
        if (/^(ni_)*altHypo$/.test(r_id_name) |
            /^(ni_)*median_rate_type$/.test(r_id_name) |
            /^(ni_)*median_rate_y$/.test(r_id_name) )
        {
            out = 'NA';
        }
    }
    //
    if (dest == 't2e_end_life_test' | dest == 'ni_t2e_end') {
        if (local_include == 'yes') {
            //
            if (/^(ni_)*unif_noncn_ex$/.test(r_id_name) | 
                /^(ni_)*recruit_noncn_ex$/.test(r_id_name) |
                /^(ni_)*enroll_len_out$/.test(r_id_name) |
                /^(ni_)*fu_len_out$/.test(r_id_name) |
                /^(ni_)*unif_cn_out$/.test(r_id_name) | 
                /^(ni_)*recruit_cn_out$/.test(r_id_name) ) 
            {
                out = 'NA';
            }
            if (pattern == 'Y') {
                //
                if (/^(ni_)*unif_cn_in$/.test(r_id_name) |
                    /^(ni_)*recruit_cn_in$/.test(r_id_name) |
                    /^(ni_)*unif_noncn$/.test(r_id_name) |
                    /^(ni_)*recruit_noncn$/.test(r_id_name) ) 
                {
                    out = 'NA';
                }
            } else if (pattern == 'N') {
                //
                if (/^(ni_)*unif_global$/.test(r_id_name) |
                    /^(ni_)*recruit_global$/.test(r_id_name) )
                {
                    out = 'NA';
                }
            }
        } else if (local_include == 'part') {
            //
            if (/^(ni_)*unif_noncn_ex$/.test(r_id_name) |
                /^(ni_)*recruit_noncn_ex$/.test(r_id_name) )
                {
                    out = 'NA';
                }
            if (pattern == 'Y') {
                //
                if (/^(ni_)*unif_cn_in$/.test(r_id_name) |
                    /^(ni_)*recruit_cn_in$/.test(r_id_name) |
                    /^(ni_)*unif_noncn$/.test(r_id_name) |
                    /^(ni_)*recruit_noncn$/.test(r_id_name) )
                    {
                        out = 'NA';
                    }
            } else if (pattern == 'N') {
                //
                if (/^(ni_)*unif_global$/.test(r_id_name) |
                    /^(ni_)*recruit_global$/.test(r_id_name) )
                    {
                        out = 'NA';
                    }
            }
        } else if (local_include == 'no') {
            //
            if (/^(ni_)*pattern$/.test(r_id_name) | 
                /^(ni_)*unif_global$/.test(r_id_name) |
                /^(ni_)*recruit_global$/.test(r_id_name) |
                /^(ni_)*enroll_st_cn$/.test(r_id_name) |
                /^(ni_)*unif_cn_in$/.test(r_id_name) |
                /^(ni_)*recruit_cn_in$/.test(r_id_name) | 
                /^(ni_)*unif_noncn$/.test(r_id_name) |
                /^(ni_)*recruit_noncn$/.test(r_id_name) ) 
                {
                    out = 'NA';
                }
        }
    }
    return out;
}

//
function Singleton_Proxy(className) {
    let instance = null;
    return new Proxy(className, {
        construct(target, args) {
            class Proxy_Class {
                constructor() {
                    if (instance === null) {
                        instance = new target(...args);
                    }
                    return instance;
                }
            }
            return new Proxy_Class();
        },
    });
}

const create_global_info = Singleton_Proxy(Ui_Input);
const create_global_info_mcpmod = Singleton_Proxy(Ui_Input);
const create_local_info = Singleton_Proxy(Ui_Input);
const create_local_info_mcpmod = Singleton_Proxy(Ui_Input);
const create_simulation_info = Singleton_Proxy(Ui_Input);
const create_con_end_t_test = Singleton_Proxy(Ui_Input);
const create_bin_end_chi_square = Singleton_Proxy(Ui_Input);
const create_t2e_end_life_test = Singleton_Proxy(Ui_Input);
const create_ni_con_end = Singleton_Proxy(Ui_Input);
const create_ni_bin_end = Singleton_Proxy(Ui_Input);
const create_ni_t2e_end = Singleton_Proxy(Ui_Input);
const create_mcpmod_con_end = Singleton_Proxy(Ui_Input);
const create_mcpmod_bin_end = Singleton_Proxy(Ui_Input);
const global_info = new create_global_info("global_info");
const global_info_mcpmod = new create_global_info_mcpmod("global_info_mcpmod");
const local_info = new create_local_info("local_info");
const local_info_mcpmod = new create_local_info_mcpmod("local_info_mcpmod");
const simulation_info = new create_simulation_info("simulation_info");
const con_end_t_test = new create_con_end_t_test("con_end_t_test");
const bin_end_chi_square = new create_bin_end_chi_square("bin_end_chi_square");
const t2e_end_life_test = new create_t2e_end_life_test("t2e_end_life_test");
const ni_con_end = new create_ni_con_end("ni_con_end");
const ni_bin_end = new create_ni_bin_end("ni_bin_end");
const ni_t2e_end = new create_ni_t2e_end("ni_t2e_end");
const mcpmod_con_end = new create_mcpmod_con_end("mcpmod_con_end");
const mcpmod_bin_end = new create_mcpmod_bin_end("mcpmod_bin_end");

local_info.transloate_pct_seq = function () {
    let pct_in_seq_r = 'NA';
    let pct_seq_r = 'NA';
    let pct_r = 'NA';
    let pct_seq_raw = 'NA';
    let pct_in_seq_raw = 'NA';
    let pct_raw = 'NA';
    let pct_seq_track = 'NA';
    let pct_in_seq_track = 'NA';
    if (this.values['chinese_all_in'] =='yes' | this.values['chinese_all_in'] =='no') {
        pct_seq_r = get_pct_sequence(
            this.values['raw_pct_seq_min'], this.values['raw_pct_seq_max'],
            this.values['raw_pct_seq_step']
        );
        pct_seq_raw = '0,100,' + this.values['raw_pct_seq_min'] + ',' +
                       this.values['raw_pct_seq_max'] + ',"double"';
        if (/seq/.test(pct_seq_r)) {
            pct_seq_track = '(' +  this.values['raw_pct_seq_min'] +
                             '% - ' + this.values['raw_pct_seq_max'] + 
                             '%, by ' + this.values['raw_pct_seq_step'] + '%)';
        } else pct_seq_track = this.values['raw_pct_seq_min'] + '%';
    } else if (this.values['chinese_all_in'] =='part') {
        pct_r = this.values['raw_pct'] / 100;
        pct_raw = this.values['raw_pct'];
        pct_in_seq_r = get_pct_sequence(
            this.values['raw_pct_in_seq_min'], this.values['raw_pct_in_seq_max'],
            this.values['raw_pct_in_seq_step']
        );
        pct_in_seq_raw = '0,100,' + this.values['raw_pct_in_seq_min'] + ',' +
                            this.values['raw_pct_in_seq_max'] + ',"double"';
        if (/seq/.test(pct_in_seq_r)) {
            pct_in_seq_track = '(' + this.values['raw_pct_in_seq_min'] +
                            '% - ' + this.values['raw_pct_in_seq_max'] + 
                            '%, by ' + this.values['raw_pct_in_seq_step'] + '%)';
        } else pct_in_seq_track = this.values['raw_pct_in_seq_min'] + '%';
    }
    this.set_value ('std_pct_in_seq', [pct_in_seq_r, pct_in_seq_track, pct_in_seq_raw]);
    this.set_value ('std_pct_seq', [pct_seq_r, pct_seq_track, pct_seq_raw]);
    this.set_value ('std_pct', [pct_r, pct_raw, pct_raw]);
}

//
local_info.calculate_sample_size = function () {
    //console.log('call local_info.calculate_sample_size');
    let chinese_all_in, pct_seq, pct_in_seq, pct;
    let id, sample, formula;
    let size_formula = [
        ['N','N - Math.ceil(N * pct_seq)','Math.ceil(N * pct_seq)','Math.ceil(N * pct_seq)'                    ,'0'                                                             ],
        ['N','N - Math.ceil(N * pct)'    ,'Math.ceil(N * pct)'    ,'Math.ceil(Math.ceil(N * pct) * pct_in_seq)','Math.ceil(N * pct) - Math.ceil(Math.ceil(N *pct) * pct_in_seq)'],
        ['N','N'                         ,'Math.ceil(N * pct_seq)','0'                                         ,'Math.ceil(N * pct_seq)'                                        ]
    ];
    chinese_all_in = this.values['chinese_all_in'];
    pct_seq = Math.round(this.values['raw_pct_seq_min'] * 10000) / 1000000;
    pct_in_seq = Math.round(this.values['raw_pct_in_seq_min'] * 10000) / 1000000;
    pct = Math.round(this.values['raw_pct'] * 10000) / 1000000;
    //console.log('pct_seq: ' + pct_seq + ', pct_in_seq: ' + pct_in_seq + ', pct: ' + pct);
    if (chinese_all_in == 'yes') id = 0;
    else if (chinese_all_in == 'part') id = 1;
    else if (chinese_all_in == 'no') id = 2;
    for (var i = 0; i < 5; i++) {
        formula = 'let str = "' + size_formula[id][i] + '";' +
                  'str = str.replaceAll("pct_seq",' + pct_seq + ');' +
                  'str = str.replaceAll("pct_in_seq",' + pct_in_seq + ');' +
                  'str = str.replaceAll("pct",' + pct + ');' +
                  'return str;';
        //console.log(formula);
        formula = new Function('pct_seq', 'pct_in_seq', 'pct', formula); 
        sample_formula[i] = formula(pct_seq, pct_in_seq, pct);
    }
    //console.log(sample_formula);
}

//
function get_sample_size (n, formula_str) {
    //console.log('call get_sample_size');
    //console.log('N: ' + n + ', formula_str: ' + formula_str);
    let add_n_formula = 'let str = "' + formula_str + '"; ' +
                        'str = str.replaceAll("N",' + n + '); ' +
                        'return str;';
    //console.log(add_n_formula);
    add_n_formula_func = new Function('n', add_n_formula);
    let final_formula_str = add_n_formula_func(n);
    final_formula_str = 'let out = ' + final_formula_str + '; return out;';
    //console.log(final_formula_str);
    final_formula_func = new Function(final_formula_str);
    let sample_size = final_formula_func(); 
    //console.log(sample_size);
    return sample_size;
}

function get_pct_sequence (start, end, step) {
    let out;
    if (start == end) out = (start / 100).toFixed(3).toString();
    else {
        out = (start / 100).toFixed(3).toString() + ', ' +
              (end / 100).toFixed(3).toString() + ', by = ' +
              (step / 100).toFixed(3).toString();
        out = 'seq(' + out + ')';
    }
    //console.log('get_pct_sequence():');
    //console.log(out);
    return out;
}

// Object Array
const ui_map_control = [
    // Sidebar Box 1a: Global study
    {ui_id:'ui_number_global_totaln'            ,r_group:global_info       ,r_id:'totalN'             ,r_id_alias:'n'               ,type:'number' ,init:554                   ,label:'Sample size'},
    {ui_id:'ui_number_gloabl_ratio'             ,r_group:global_info       ,r_id:'ratio'              ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Randomization ratio'},
    {ui_id:'ui_select_global_alt_hypo'          ,r_group:global_info       ,r_id:'altHypo'            ,r_id_alias:null              ,type:'string' ,init:'t'                   ,label:'Alternative hypothesis'},
    {ui_id:'ui_number_sig_level'                ,r_group:global_info       ,r_id:'SigL'               ,r_id_alias:null              ,type:'number' ,init:0.05                  ,label:'Significance level'},
    {ui_id:'ui_number_ni_nim'                   ,r_group:global_info       ,r_id:'nim'                ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Non-infereriority margin'},
    {ui_id:'ui_select_ni_hma'                   ,r_group:global_info       ,r_id:'hma'                ,r_id_alias:null              ,type:'num2str',init:1                     ,label:'Higher means are'},
    // Sidebar Box 2a: Subpopulation 
    {ui_id:'ui_number_local_ratio'              ,r_group:local_info        ,r_id:'ratio_cn'           ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Randomization ratio'},
    {ui_id:'ui_number_local_thd'                ,r_group:local_info        ,r_id:'thd'                ,r_id_alias:null              ,type:'number' ,init:0.5                   ,label:'Proportion of the overall treatment effect preserved'},
    {ui_id:'ui_select_local_include'            ,r_group:local_info        ,r_id:'chinese_all_in'     ,r_id_alias:null              ,type:'string' ,init:'yes'                 ,label:'Are all included in the global study?'},
    {ui_id:'ui_text_local_pct_seq'              ,r_group:local_info        ,r_id:'raw_pct_seq'        ,r_id_alias:null              ,type:'object' ,init:[0,100,10,30,"double"],label:'N of Subpopulation /N of Global (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'raw_pct_seq_min'    ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'N of Subpopulation /N of Global (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'raw_pct_seq_max'    ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'N of Subpopulation /N of Global (%)'},
    {ui_id:'ui_number_local_pct_seq_step'       ,r_group:local_info        ,r_id:'raw_pct_seq_step'   ,r_id_alias:null              ,type:'number' ,init:5                     ,label:'Step (%)'},
    {ui_id:'ui_number_local_pct'                ,r_group:local_info        ,r_id:'raw_pct'            ,r_id_alias:null              ,type:'number' ,init:30                    ,label:'N of Subpopulation /N of Global (%)'},
    {ui_id:'ui_text_local_pct_in_seq'           ,r_group:local_info        ,r_id:'raw_pct_in_seq'     ,r_id_alias:null              ,type:'object' ,init:[0,100,20,50,"double"],label:'N of Subpopulation within Global/ N of Subpopulation (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'raw_pct_in_seq_min' ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'N of Subpopulation within Global/ N of Subpopulation (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'raw_pct_in_seq_max' ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'N of Subpopulation within Global/ N of Subpopulation (%)'},
    {ui_id:'ui_number_local_pct_in_seq_step'    ,r_group:local_info        ,r_id:'raw_pct_in_seq_step',r_id_alias:null              ,type:'number' ,init:5                     ,label:'Step (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'std_pct_seq'        ,r_id_alias:'pct_seq'         ,type:'object' ,init:null                  ,label:'N of Subpopulation /N of Global (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'std_pct_in_seq'     ,r_id_alias:'pct_in_seq'      ,type:'object' ,init:null                  ,label:'N of Subpopulation within Global/ N of Subpopulation (%)'},
    {ui_id:null                                 ,r_group:local_info        ,r_id:'std_pct'            ,r_id_alias:'pct'             ,type:'object' ,init:null                  ,label:'N of Subpopulation /N of Global (%)'},
    // Sidebar Box 1b: MCP-Mod Global study
    {ui_id:'ui_number_mcpmod_global_alpha'      ,r_group:global_info_mcpmod,r_id:'global_alpha'       ,r_id_alias:'alpha'           ,type:'number' ,init:0.05                  ,label:'Alpha'},
    {ui_id:'ui_select_mcpmod_global_direction'  ,r_group:global_info_mcpmod,r_id:'global_direction'   ,r_id_alias:'direction'       ,type:'string' ,init:'increasing'          ,label:'Direction'},
    {ui_id:null                                 ,r_group:global_info_mcpmod,r_id:'global_doses'       ,r_id_alias:'doses'           ,type:'object' ,init:null                  ,label:'Dosage in group'},
    {ui_id:null                                 ,r_group:global_info_mcpmod,r_id:'global_n_arm'       ,r_id_alias:'n_arm'           ,type:'object' ,init:null                  ,label:'Number of patients'},
    // Sidebar Box 2b: MCP-Mod Subpopulation 
    {ui_id:'ui_number_mcpmod_local_pi'          ,r_group:local_info_mcpmod ,r_id:'local_pi'           ,r_id_alias:'pi'              ,type:'number' ,init:0.5                   ,label:'Proportion of the overall treatment effect preserved'},
    {ui_id:null                                 ,r_group:local_info_mcpmod ,r_id:'local_n_arm_reg'    ,r_id_alias:'n_arm_reg'       ,type:'object' ,init:null                  ,label:'Subpopulation patients'},
    // Sidebar Box 3: Simulation options
    {ui_id:'ui_number_option_simn'              ,r_group:simulation_info   ,r_id:'simN'               ,r_id_alias:'nsim'            ,type:'number' ,init:1000                  ,label:'Number of simulations'},
    {ui_id:'ui_number_option_seed'              ,r_group:simulation_info   ,r_id:'seed'               ,r_id_alias:null              ,type:'number' ,init:now                   ,label:'Seed'},
    // Body Tabpane 1: Continuous Endpoint - T Test
    {ui_id:'ui_number_t_test_mean_trt'          ,r_group:con_end_t_test    ,r_id:'mean_trt'           ,r_id_alias:null              ,type:'number' ,init:3.5                   ,label:'Mean of experimental arm'},
    {ui_id:'ui_number_t_test_sd_trt'            ,r_group:con_end_t_test    ,r_id:'sd_trt'             ,r_id_alias:null              ,type:'number' ,init:2.5                   ,label:'Standard deviation of experimental arm'},
    {ui_id:'ui_number_t_test_mean_ctrl'         ,r_group:con_end_t_test    ,r_id:'mean_ctrl'          ,r_id_alias:null              ,type:'number' ,init:4.5                   ,label:'Mean of control arm'},
    {ui_id:'ui_number_t_test_sd_ctrl'           ,r_group:con_end_t_test    ,r_id:'sd_ctrl'            ,r_id_alias:null              ,type:'number' ,init:2.5                   ,label:'Standard deviation of control arm'},
    // Body Tabpane 2: Binary Endpoint - Chi-square
    {ui_id:'ui_number_chi_square_prop_trt'      ,r_group:bin_end_chi_square,r_id:'prop_trt'           ,r_id_alias:null              ,type:'number' ,init:0.3                   ,label:'Proportion of experimental arm'},
    {ui_id:'ui_number_chi_square_prop_ctrl'     ,r_group:bin_end_chi_square,r_id:'prop_ctrl'          ,r_id_alias:null              ,type:'number' ,init:0.4                   ,label:'Proportion of control arm'},
    // Body Tabpane 3: Time-to-event Endpoint - Life Test
    {ui_id:'ui_number_life_test_enroll_len'     ,r_group:t2e_end_life_test ,r_id:'enroll_len'         ,r_id_alias:null              ,type:'number' ,init:15                    ,label:'Accrual time (months)'},
    {ui_id:'ui_number_life_test_n_targetevent'  ,r_group:t2e_end_life_test ,r_id:'n_targetevent'      ,r_id_alias:null              ,type:'number' ,init:227                   ,label:'N of target event'},
    {ui_id:'ui_radio_life_test_median_type_y'   ,r_group:t2e_end_life_test ,r_id:'median_rate_type'   ,r_id_alias:null              ,type:'number' ,init:0                     ,label:''},
    {ui_id:'ui_radio_life_test_median_type_m'   ,r_group:t2e_end_life_test ,r_id:'median_rate_type'   ,r_id_alias:null              ,type:'number' ,init:1                     ,label:''},
    {ui_id:'ui_number_life_test_median_rate_y'  ,r_group:t2e_end_life_test ,r_id:'median_rate_y'      ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'Control yearly rate'},
    {ui_id:'ui_number_life_test_median_rate_m'  ,r_group:t2e_end_life_test ,r_id:'median_rate_m'      ,r_id_alias:'ms0'             ,type:'number' ,init:20                    ,label:'Control median (months)'},
    {ui_id:'ui_number_life_test_drop_ctrl'      ,r_group:t2e_end_life_test ,r_id:'drop_ctrl'          ,r_id_alias:null              ,type:'number' ,init:0.2                   ,label:'Control yearly dropout rate'},
    {ui_id:'ui_number_life_test_drop_trt'       ,r_group:t2e_end_life_test ,r_id:'drop_trt'           ,r_id_alias:null              ,type:'number' ,init:0.2                   ,label:'Experimental yearly dropout rate'},
    {ui_id:'ui_number_life_test_hr'             ,r_group:t2e_end_life_test ,r_id:'hr'                 ,r_id_alias:null              ,type:'number' ,init:0.625                 ,label:'Target HR'},
    {ui_id:'ui_number_life_test_pattern'        ,r_group:t2e_end_life_test ,r_id:'pattern'            ,r_id_alias:null              ,type:'num2str',init:1                     ,label:'Pattern: Subpopulation within global study has the same recruitment pattern with global study'},
    {ui_id:'ui_number_life_test_unif_global'    ,r_group:t2e_end_life_test ,r_id:'unif_global'        ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:t2e_end_life_test ,r_id:'recruit_global'     ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'Global study: Customized recruitment'},
    {ui_id:'ui_number_life_test_enroll_st_cn'   ,r_group:t2e_end_life_test ,r_id:'enroll_st_cn'       ,r_id_alias:null              ,type:'number' ,init:3                     ,label:'Subpopulation within global study: Starting point (months)'},
    {ui_id:'ui_number_life_test_unif_cn_in'     ,r_group:t2e_end_life_test ,r_id:'unif_cn_in'         ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Subpopulation within global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:t2e_end_life_test ,r_id:'recruit_cn_in'      ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'Subpopulation within global study: Customized recruitment'},
    {ui_id:'ui_number_life_test_unif_noncn'     ,r_group:t2e_end_life_test ,r_id:'unif_noncn'         ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Global participants excluding subpopulation: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:t2e_end_life_test ,r_id:'recruit_noncn'      ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'Global participants excluding subpopulation: Customized recruitment'},
    {ui_id:'ui_number_life_test_unif_noncn_ex'  ,r_group:t2e_end_life_test ,r_id:'unif_noncn_ex'      ,r_id_alias:'unif_noncn'      ,type:'number' ,init:1                     ,label:'Global participants: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:t2e_end_life_test ,r_id:'recruit_noncn_ex'   ,r_id_alias:'recruit_noncn'   ,type:'object' ,init:null                  ,label:'Global participants: Customized recruitment'},
    {ui_id:'ui_number_life_test_enroll_len_out' ,r_group:t2e_end_life_test ,r_id:'enroll_len_out'     ,r_id_alias:null              ,type:'number' ,init:15                    ,label:'Subpopulation out of global study: Accrual time (months)'}, 
    {ui_id:'ui_number_life_test_fu_len_out'     ,r_group:t2e_end_life_test ,r_id:'fu_len_out'         ,r_id_alias:null              ,type:'number' ,init:24                    ,label:'Subpopulation out of global study: Follow up time (months)'}, 
    {ui_id:'ui_number_life_test_unif_cn_out'    ,r_group:t2e_end_life_test ,r_id:'unif_cn_out'        ,r_id_alias:null              ,type:'number' ,init:1                     ,label:'Subpopulation out of global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:t2e_end_life_test ,r_id:'recruit_cn_out'     ,r_id_alias:null              ,type:'object' ,init:null                  ,label:'Subpopulation out of global study: Customized recruitment'},
    // Body Tabpane 4: Non-inferiority - Continuous Endpoint
    {ui_id:'ui_number_ni_con_end_mean_trt'      ,r_group:ni_con_end        ,r_id:'ni_mean_trt'        ,r_id_alias:'mean_trt'        ,type:'number' ,init:3.5                   ,label:'Mean of experimental arm'},
    {ui_id:'ui_number_ni_con_end_sd_trt'        ,r_group:ni_con_end        ,r_id:'ni_sd_trt'          ,r_id_alias:'sd_trt'          ,type:'number' ,init:2.5                   ,label:'Standard deviation of experimental arm'},
    {ui_id:'ui_number_ni_con_end_mean_ctrl'     ,r_group:ni_con_end        ,r_id:'ni_mean_ctrl'       ,r_id_alias:'mean_ctrl'       ,type:'number' ,init:4.5                   ,label:'Mean of control arm'},
    {ui_id:'ui_number_ni_con_end_sd_ctrl'       ,r_group:ni_con_end        ,r_id:'ni_sd_ctrl'         ,r_id_alias:'sd_ctrl'         ,type:'number' ,init:2.5                   ,label:'Standard deviation of control arm'},
    // Body Tabpane 5: Non-inferiority - Binary Endpoint
    {ui_id:'ui_number_ni_bin_end_prop_trt'      ,r_group:ni_bin_end        ,r_id:'ni_prop_trt'        ,r_id_alias:'prop_trt'        ,type:'number' ,init:0.3                   ,label:'Proportion of experimental arm'},
    {ui_id:'ui_number_ni_bin_end_prop_ctrl'     ,r_group:ni_bin_end        ,r_id:'ni_prop_ctrl'       ,r_id_alias:'prop_ctrl'       ,type:'number' ,init:0.4                   ,label:'Proportion of control arm'},
    // Body Tabpane 6: Non-inferiority - Time-to-event Endpoint
    {ui_id:'ui_number_ni_t2e_end_enroll_len'    ,r_group:ni_t2e_end        ,r_id:'ni_enroll_len'      ,r_id_alias:'enroll_len'      ,type:'number' ,init:15                    ,label:'Accrual time (months)'},
    {ui_id:'ui_number_ni_t2e_end_n_targetevent' ,r_group:ni_t2e_end        ,r_id:'ni_n_targetevent'   ,r_id_alias:'n_targetevent'   ,type:'number' ,init:227                   ,label:'N of target event'},
    {ui_id:'ui_radio_ni_t2e_end_median_type_y'  ,r_group:ni_t2e_end        ,r_id:'ni_median_rate_type',r_id_alias:'median_rate_type',type:'number' ,init:0                     ,label:''},
    {ui_id:'ui_radio_ni_t2e_end_median_type_m'  ,r_group:ni_t2e_end        ,r_id:'ni_median_rate_type',r_id_alias:'median_rate_type',type:'number' ,init:1                     ,label:''},
    {ui_id:'ui_number_ni_t2e_end_median_rate_y' ,r_group:ni_t2e_end        ,r_id:'ni_median_rate_y'   ,r_id_alias:'median_rate_y'   ,type:'object' ,init:null                  ,label:'Control yearly rate'},
    {ui_id:'ui_number_ni_t2e_end_median_rate_m' ,r_group:ni_t2e_end        ,r_id:'ni_median_rate_m'   ,r_id_alias:'ms0'             ,type:'number' ,init:20                    ,label:'Control median (months)'},
    {ui_id:'ui_number_ni_t2e_end_drop_ctrl'     ,r_group:ni_t2e_end        ,r_id:'ni_drop_ctrl'       ,r_id_alias:'drop_ctrl'       ,type:'number' ,init:0.2                   ,label:'Control yearly dropout rate'},
    {ui_id:'ui_number_ni_t2e_end_drop_trt'      ,r_group:ni_t2e_end        ,r_id:'ni_drop_trt'        ,r_id_alias:'drop_trt'        ,type:'number' ,init:0.2                   ,label:'Experimental yearly dropout rate'},
    {ui_id:'ui_number_ni_t2e_end_hr'            ,r_group:ni_t2e_end        ,r_id:'ni_hr'              ,r_id_alias:'hr'              ,type:'number' ,init:0.625                 ,label:'Target HR'},
    {ui_id:'ui_number_ni_t2e_end_pattern'       ,r_group:ni_t2e_end        ,r_id:'ni_pattern'         ,r_id_alias:'pattern'         ,type:'num2str',init:1                     ,label:'Pattern: Subpopulation within global study has the same recruitment pattern with global study'},
    {ui_id:'ui_number_ni_t2e_end_unif_global'   ,r_group:ni_t2e_end        ,r_id:'ni_unif_global'     ,r_id_alias:'unif_global'     ,type:'number' ,init:1                     ,label:'Global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:ni_t2e_end        ,r_id:'ni_recruit_global'  ,r_id_alias:'recruit_global'  ,type:'object' ,init:null                  ,label:'Global study: Customized recruitment'},
    {ui_id:'ui_number_ni_t2e_end_enroll_st_cn'  ,r_group:ni_t2e_end        ,r_id:'ni_enroll_st_cn'    ,r_id_alias:'enroll_st_cn'    ,type:'number' ,init:3                     ,label:'Subpopulation within global study: Starting point (months)'},
    {ui_id:'ui_number_ni_t2e_end_unif_cn_in'    ,r_group:ni_t2e_end        ,r_id:'ni_unif_cn_in'      ,r_id_alias:'unif_cn_in'      ,type:'number' ,init:1                     ,label:'Subpopulation within global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:ni_t2e_end        ,r_id:'ni_recruit_cn_in'   ,r_id_alias:'recruit_cn_in'   ,type:'object' ,init:null                  ,label:'Subpopulation within global study: Customized recruitment'},
    {ui_id:'ui_number_ni_t2e_end_unif_noncn'    ,r_group:ni_t2e_end        ,r_id:'ni_unif_noncn'      ,r_id_alias:'unif_noncn'      ,type:'number' ,init:1                     ,label:'Global participants excluding subpopulation: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:ni_t2e_end        ,r_id:'ni_recruit_noncn'   ,r_id_alias:'recruit_noncn'   ,type:'object' ,init:null                  ,label:'Global participants excluding subpopulation: Customized recruitment'},
    {ui_id:'ui_number_ni_t2e_end_unif_noncn_ex' ,r_group:ni_t2e_end        ,r_id:'ni_unif_noncn_ex'   ,r_id_alias:'unif_noncn'      ,type:'number' ,init:1                     ,label:'Global participants: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:ni_t2e_end        ,r_id:'ni_recruit_noncn_ex',r_id_alias:'recruit_noncn'   ,type:'object' ,init:null                  ,label:'Global participants: Customized recruitment'},
    {ui_id:'ui_number_ni_t2e_end_enroll_len_out',r_group:ni_t2e_end        ,r_id:'ni_enroll_len_out'  ,r_id_alias:'enroll_len_out'  ,type:'number' ,init:15                    ,label:'Subpopulation out of global study: Accrual time (months)'}, 
    {ui_id:'ui_number_ni_t2e_end_fu_len_out'    ,r_group:ni_t2e_end        ,r_id:'ni_fu_len_out'      ,r_id_alias:'fu_len_out'      ,type:'number' ,init:24                    ,label:'Subpopulation out of global study: Follow up time (months)'}, 
    {ui_id:'ui_number_ni_t2e_end_unif_cn_out'   ,r_group:ni_t2e_end        ,r_id:'ni_unif_cn_out'     ,r_id_alias:'unif_cn_out'     ,type:'number' ,init:1                     ,label:'Subpopulation out of global study: Uniform recruitment?'},
    {ui_id:null                                 ,r_group:ni_t2e_end        ,r_id:'ni_recruit_cn_out'  ,r_id_alias:'recruit_cn_out'  ,type:'object' ,init:null                  ,label:'Subpopulation out of global study: Customized recruitment'},
    // Body Tabpane 7: MCP-Mod - Continuous Endpoint
    {ui_id:'ui_number_mcpmod_con_end_e0'        ,r_group:mcpmod_con_end    ,r_id:'con_e0'             ,r_id_alias:'e0'              ,type:'number' ,init:1.25                  ,label:'E0'}, 
    {ui_id:'ui_number_mcpmod_con_end_emax'      ,r_group:mcpmod_con_end    ,r_id:'con_emax'           ,r_id_alias:'emax'            ,type:'number' ,init:0.15                  ,label:'Emax'},
    {ui_id:'ui_number_mcpmod_con_end_sigma'     ,r_group:mcpmod_con_end    ,r_id:'con_sigma'          ,r_id_alias:'sigma'           ,type:'number' ,init:0.34                  ,label:'Standard deviation'},
    {ui_id:'ui_number_mcpmod_con_end_delta_yes' ,r_group:mcpmod_con_end    ,r_id:'con_delta_yes'      ,r_id_alias:null              ,type:'boolean',init:0                     ,label:''},
    {ui_id:'ui_number_mcpmod_con_end_delta'     ,r_group:mcpmod_con_end    ,r_id:'con_delta'          ,r_id_alias:'delta'           ,type:'number' ,init:0                     ,label:'Minimal clinical meaningful treatment effect(delta)(if available)'},
    {ui_id:'ui_select_mcpmod_con_end_type'      ,r_group:mcpmod_con_end    ,r_id:'con_type'           ,r_id_alias:'type'            ,type:'num2str',init:1                     ,label:'Global power'}, 
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model1'         ,r_id_alias:'model1'          ,type:'object' ,init:null                  ,label:'Model 1'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model2'         ,r_id_alias:'model2'          ,type:'object' ,init:null                  ,label:'Model 2'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model3'         ,r_id_alias:'model3'          ,type:'object' ,init:null                  ,label:'Model 3'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model4'         ,r_id_alias:'model4'          ,type:'object' ,init:null                  ,label:'Model 4'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model5'         ,r_id_alias:'model5'          ,type:'object' ,init:null                  ,label:'Model 5'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model6'         ,r_id_alias:'model6'          ,type:'object' ,init:null                  ,label:'Model 6'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model7'         ,r_id_alias:'model7'          ,type:'object' ,init:null                  ,label:'Model 7'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model8'         ,r_id_alias:'model8'          ,type:'object' ,init:null                  ,label:'Model 8'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model9'         ,r_id_alias:'model9'          ,type:'object' ,init:null                  ,label:'Model 9'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_model10'        ,r_id_alias:'model10'         ,type:'object' ,init:null                  ,label:'Model 10'},
    {ui_id:null                                 ,r_group:mcpmod_con_end    ,r_id:'con_mods_str'       ,r_id_alias:'mods_str'        ,type:'object' ,init:null                  ,label:''},
    // Body Tabpane 8: MCP-Mod - Binary Endpoint
    {ui_id:'ui_number_mcpmod_bin_end_e0'        ,r_group:mcpmod_bin_end    ,r_id:'bin_e0'             ,r_id_alias:'e0'              ,type:'number' ,init:0.1                   ,label:'E0'}, 
    {ui_id:'ui_number_mcpmod_bin_end_emax'      ,r_group:mcpmod_bin_end    ,r_id:'bin_emax'           ,r_id_alias:'emax'            ,type:'number' ,init:0.35                  ,label:'Emax'},
    {ui_id:'ui_number_mcpmod_bin_end_delta_yes' ,r_group:mcpmod_bin_end    ,r_id:'bin_delta_yes'      ,r_id_alias:null              ,type:'boolean',init:0                     ,label:''},
    {ui_id:'ui_number_mcpmod_bin_end_delta'     ,r_group:mcpmod_bin_end    ,r_id:'bin_delta'          ,r_id_alias:'delta'           ,type:'number' ,init:0                     ,label:'Minimal clinical meaningful treatment effect(delta)(if available)'},
    {ui_id:'ui_select_mcpmod_bin_end_type'      ,r_group:mcpmod_bin_end    ,r_id:'bin_type'           ,r_id_alias:'type'            ,type:'num2str',init:1                     ,label:'Global power'}, 
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model1'         ,r_id_alias:'model1'          ,type:'object' ,init:null                  ,label:'Model 1'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model2'         ,r_id_alias:'model2'          ,type:'object' ,init:null                  ,label:'Model 2'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model3'         ,r_id_alias:'model3'          ,type:'object' ,init:null                  ,label:'Model 3'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model4'         ,r_id_alias:'model4'          ,type:'object' ,init:null                  ,label:'Model 4'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model5'         ,r_id_alias:'model5'          ,type:'object' ,init:null                  ,label:'Model 5'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model6'         ,r_id_alias:'model6'          ,type:'object' ,init:null                  ,label:'Model 6'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model7'         ,r_id_alias:'model7'          ,type:'object' ,init:null                  ,label:'Model 7'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model8'         ,r_id_alias:'model8'          ,type:'object' ,init:null                  ,label:'Model 8'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model9'         ,r_id_alias:'model9'          ,type:'object' ,init:null                  ,label:'Model 9'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_model10'        ,r_id_alias:'model10'         ,type:'object' ,init:null                  ,label:'Model 10'},
    {ui_id:null                                 ,r_group:mcpmod_bin_end    ,r_id:'bin_mods_str'       ,r_id_alias:'mods_str'        ,type:'object' ,init:null                  ,label:''},
];

const create_map_control = Singleton_Proxy(Map_Control);
const map_control = new create_map_control();
//console.log(map_control);

map_control.set_value(ui_map_control);
const r_id_groups = map_control.transfer_to_group();
const r_id_labels = map_control.transfer_to_label();
const r_id_ui_ids = map_control.transfer_to_label(id = 'r_id', description = 'ui_id');

const create_doselevel_control = Singleton_Proxy(Doselevel_Control);
const doselevel_control = new create_doselevel_control();
//console.log(doselevel_control);

const create_con_model_control = Singleton_Proxy(Con_Model_Control);
const con_model_control = new create_con_model_control();
//console.log(con_model_control);

const create_bin_model_control = Singleton_Proxy(Bin_Model_Control);
const bin_model_control = new create_bin_model_control();
//console.log(bin_model_control);

function change_value_type (value) {
    let out;
    if (parseFloat(value).toString() == 'NaN') out = value;
    else out = parseFloat(value);
    return out;
}

function Ui_Widget (ui_id, r_id, init) {
    this.ui_id = ui_id;
    this.r_id = r_id;
    this.init = init;
}
    
Ui_Widget.prototype = {
    constructor: Ui_Widget,
    get_ui_id: function () {
        return document.getElementById(this.ui_id);
    },
    set_ui_init_value: function (element_id) {
        const type = element_id.type;
        const id_group_ref = r_id_groups.get(this.r_id);
        //console.log("Set_ui_init_value: " + this.ui_id + " " + type + " " + this.init);
        if (type == "number" | type == "select-one") {
            element_id.value = this.init;
            id_group_ref.set_value(this.r_id, this.init);
            element_id.dispatchEvent(new Event("change"));
            //
        } else if (type == "radio") {
            if (this.init == 1) {
                element_id.setAttribute("checked", "checked");
                id_group_ref.set_value(this.r_id, element_id.value);
                element_id.dispatchEvent(new Event("click"));
            }
        } else if (type == "checkbox") {
            if (this.init == 1) {
                element_id.checked = true;
                id_group_ref.set_value(this.r_id, true);
                element_id.dispatchEvent(new Event("click"));
            } else if (this.init == 0) {
                element_id.checked = false;
                id_group_ref.set_value(this.r_id, false);
            }
        } else if (type == "text") {
            if (this.r_id == "raw_pct_seq" | this.r_id == "raw_pct_in_seq") {
                let defaults = this.init;
                if (typeof(defaults) == 'string') defaults = defaults.split(',');
                const varname = this.r_id;
                const uiname = this.ui_id;
                const uistep = 'ui_number' + uiname.substr(7) + '_step';
                //console.log("text:" + element_id + " " + this.r_id);
                // ion-rangeslider: setup and method
                $("#" + uiname).ionRangeSlider({
                    min:  defaults[0],
                    max:  defaults[1],
                    //from: defaults[2],
                    //to:   defaults[3],
                    type: defaults[4],
                    step: 0.1,
                    grid: true,
                    onStart: function (data) {
                        id_group_ref.set_value(varname + '_min', data.from);
                        id_group_ref.set_value(varname + '_max', data.to);
                    },
                    onFinish: function (data) {
                        id_group_ref.set_value(varname + '_min', data.from);
                        id_group_ref.set_value(varname + '_max', data.to);
                        if (data.from == data.to) {
                            $('#' + uistep).attr('disabled', true);
                            $('#' + uistep).val(0);
                        } else {
                            $('#' + uistep).attr('disabled', false);
                            $('#' + uistep).val(5);
                        }
                    }
                });
                let slider = $("#" + uiname).data("ionRangeSlider");
                slider.update({
                    from: defaults[2],
                    to:   defaults[3]
                });
                id_group_ref.set_value(varname + '_min', defaults[2]);
                id_group_ref.set_value(varname + '_max', defaults[3]);
            }
        } else if (type == "hidden") {
            element_id.value = this.init;
            id_group_ref.set_value(this.r_id, this.init);
            if (this.init == 1) {
                $('#' + this.ui_id + '_y').removeClass('notActive');
                $('#' + this.ui_id + '_y').addClass('active');
                $('#' + this.ui_id + '_n').removeClass('active');
                $('#' + this.ui_id + '_n').addClass('notActive');
            } else if (this.init == 0) {
                $('#' + this.ui_id + '_y').removeClass('active');
                $('#' + this.ui_id + '_y').addClass('notActive');
                $('#' + this.ui_id + '_n').removeClass('notActive');
                $('#' + this.ui_id + '_n').addClass('active');
            }
            element_id.dispatchEvent(new Event("change"));
            if (/^(ni_)*unif_global$/.test(this.r_id) |
                /^(ni_)*unif_cn_in$/.test(this.r_id) |
                /^(ni_)*unif_noncn$/.test(this.r_id) |
                /^(ni_)*unif_noncn_ex$/.test(this.r_id) |
                /^(ni_)*unif_cn_out$/.test(this.r_id) ) 
            {
                let recruit_varname = this.r_id.replace(/unif_/, 'recruit_');
                //
                id_group_ref.set_value(recruit_varname, [undefined, 'NA']);
            }
        }
    },
    //
    listen_ui_click: function (element_id) {
        const r_id = this.r_id;
        const type = element_id.type;
        const id_group_ref = r_id_groups.get(this.r_id);
        let ui_value;
        //console.log("Set_ui_listener: " + this.ui_id + " " + this.r_id + " " + type);
        if (type == "number" | type == "select-one") {
            element_id.addEventListener("change", function() {
                ui_input_change_flag = 1;
                //
                ui_value = element_id.value;
                id_group_ref.set_value(r_id, change_value_type(element_id.value));
                let tmp_value;
                if (/^(ni_)*median_rate_m$/.test(r_id)) {
                    // Month rate
                    tmp_value = (1 - Math.exp(12 * Math.log(0.5) / ui_value)).toFixed(6);
                    if (r_id.substr(0, 3) != 'ni_') $('#ui_number_life_test_median_rate_y').val(tmp_value);
                    else $('#ui_number_ni_t2e_end_median_rate_y').val(tmp_value);
                    id_group_ref.set_value(r_id.replace(/_m/, '_y'), change_value_type(tmp_value));
                } else if (/^(ni_)*median_rate_y$/.test(r_id) & ui_value != null) {
                    // Year rate
                    tmp_value = (Math.log(0.5) * 12 / Math.log(1 - ui_value)).toFixed(0);
                    if (r_id.substr(0, 3) != 'ni_') $('#ui_number_life_test_median_rate_m').val(tmp_value);
                    else $('#ui_number_ni_t2e_end_median_rate_m').val(tmp_value);
                    id_group_ref.set_value(r_id.replace(/_y/, '_m'), change_value_type(tmp_value));
                } 
            });
        } else if (type == "radio") {
            element_id.addEventListener("click", function() {
                ui_input_change_flag = 1;
                id_group_ref.set_value(r_id, change_value_type(element_id.value));
                if (r_id == 'median_rate_type') {
                    let tmp_value;
                    if (element_id.value == 1) {
                        $('#ui_number_life_test_median_rate_m').attr("readonly",false);
                        $('#ui_number_life_test_median_rate_y').attr("readonly",true);
                    } else if (element_id.value == 0) {
                        $('#ui_number_life_test_median_rate_m').attr("readonly",true);
                        $('#ui_number_life_test_median_rate_y').attr("readonly",false);
                    }
                } else if (r_id == 'ni_median_rate_type') {
                    let tmp_value;
                    if (element_id.value == 1) {
                        $('#ui_number_ni_t2e_end_median_rate_m').attr("readonly",false);
                        $('#ui_number_ni_t2e_end_median_rate_y').attr("readonly",true);
                    } else if (element_id.value == 0) {
                        $('#ui_number_ni_t2e_end_median_rate_m').attr("readonly",true);
                        $('#ui_number_ni_t2e_end_median_rate_y').attr("readonly",false);
                    }
                }
            });
        } else if (type == "hidden") {
            ui_input_change_flag = 1;
            element_id.addEventListener("change", function() {
                id_group_ref.set_value(r_id, change_value_type(element_id.value));
            });
            //
            $('#' + element_id.id + '_y').on('click', function(){
                //console.log(element_id.id + '_y: click');
                let old_value, new_value;
                var sel = $(this).data('title');
                var tog = $(this).data('toggle');
                $('#'+tog).attr('value', sel);
                //console.log("current value: " + $('#'+tog).attr('value'));
                element_id.dispatchEvent(new Event("change"));
                $('a[data-toggle="'+tog+'"]').not('[data-title="'+sel+'"]').removeClass('active').addClass('notActive');
                $('a[data-toggle="'+tog+'"][data-title="'+sel+'"]').removeClass('notActive').addClass('active');
                //
                if (/^(ni_)*unif_global$/.test(r_id) |
                    /^(ni_)*unif_cn_in$/.test(r_id) |
                    /^(ni_)*unif_noncn$/.test(r_id) |
                    /^(ni_)*unif_noncn_ex$/.test(r_id) |
                    /^(ni_)*unif_cn_out$/.test(r_id) ) 
                {
                    //
                    let recruit_varname = r_id.replace(/unif_/, 'recruit_');
                    old_value = map_control.get_rid_value(recruit_varname);
                    //
                    //
                    if (old_value[0][0] != undefined) map_control.set_rid_value(recruit_varname, [undefined, old_value[0][0]]);
                    //
                    let recruit_label = tog.replace(/number/, 'label').replace(/unif_/,'recruit_');
                    $('#' + recruit_label).css('color','black');
                }
            });
            $('#' + element_id.id + '_n').on('click', function(){
                //console.log(element_id.id + '_n: click');
                var sel = $(this).data('title');
                var tog = $(this).data('toggle');
                $('#'+tog).attr('value', sel);
                //console.log("current value: " + $('#'+tog).attr('value'));
                element_id.dispatchEvent(new Event("change"));
                $('a[data-toggle="'+tog+'"]').not('[data-title="'+sel+'"]').removeClass('active').addClass('notActive');
                $('a[data-toggle="'+tog+'"][data-title="'+sel+'"]').removeClass('notActive').addClass('active');
                //
                if (/^(ni_)*unif_global$/.test(r_id) |
                    /^(ni_)*unif_cn_in$/.test(r_id) |
                    /^(ni_)*unif_noncn$/.test(r_id) |
                    /^(ni_)*unif_noncn_ex$/.test(r_id) |
                    /^(ni_)*unif_cn_out$/.test(r_id) )
                {
                    listen_ui_recruitment_button_open_dialog (this, r_id);
                }
            });
        } else if (type == "checkbox") {
            element_id.addEventListener("click", function() {
                ui_input_change_flag = 1;
                id_group_ref.set_value(r_id, element_id.checked);
            });
        }
    }
}

function set_expected_list (total, length) {
    let out = new Array();
    let average = Math.ceil(total / length);
    //console.log(average);
    for (var i = 0; i < length; i++) {
        //
        //if (total > average) out.push(average);
        //else if (total > 0) out.push(total);
        //else out.push(0);
        if (total > average) out.push('');
        else if (total > 0) out.push('');
        else out.push('');
        total-=average;
    }
    //console.log(out);
    return out;
}

function blueRowRenderer(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.background = '#DAE3F3';
    td.style.textAlign = 'right';
}

function redRowRenderer(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.textAlign = 'right';
    if (value == 0) {
        td.style.background = '#DAE3F3';
        td.style.color = 'black';
    } else {
        td.style.background = '#ED5565';
        td.style.color = 'white';
    }
}

function errorRenderer(instance, td, row, col, prop, value, cellProperties) {
    //console.log('Function - errorRenderer:');
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (value.toString().substring(0, 5) == 'Error') {
        td.style.background = '#ED5565';
        td.style.color = 'white';
    }
}

function listen_ui_recruitment_button_open_dialog (btn_id, r_id) {
    //
    let total_global, total_sub, start_sub, len_global, len_sub, pct_min, pct_max, expected_list;
    let total_global_label, len_global_label, tips0, tips1, tips2, sample_track;
    let sample_track_flag = 'simple';
    let prefix = '';
    if (r_id.substr(0, 3) == 'ni_') prefix = 'ni_';
    total_global = map_control.get_rid_value('totalN')[0];
    total_global_label = map_control.get_rid_value('totalN')[2];
    len_global = map_control.get_rid_value(prefix + 'enroll_len')[0];
    len_global_label = map_control.get_rid_value(prefix + 'enroll_len')[2];
    // calculate sample size
    local_info.calculate_sample_size();
    if (/^(ni_)*unif_global$/.test(r_id)) {
        // global_total
        // 2 Completely included - Global study: Uniform recruitment?
        //   Partially included -  Global study: Uniform recruitment?
        total_sub = total_global;
        len_sub = len_global;
        tips1 = total_global_label;
        tips2 = len_global_label;
        expected_list = set_expected_list(total_sub, len_sub);
        call_custom_recruitment_dialogue(r_id, total_sub, 1, tips1, tips2, expected_list, prefix + 'recruit_global');
    } else if (/^(ni_)*unif_cn_in$/.test(r_id)) {
        // local_in_global
        // 3 Completely included - Subpopulation within global study: Starting point (mth.)
        //   Partially included -  Subpopulation within global study: Starting point (mth.)
        if (current_local_include[0] != 'part') {
            pct_min = map_control.get_rid_value('raw_pct_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_seq_max')[0];
            tips2 = map_control.get_rid_value('raw_pct_seq_min')[2];
        } else {
            pct_min = map_control.get_rid_value('raw_pct_in_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_in_seq_max')[0];
            tips2 = map_control.get_rid_value('raw_pct')[2] + ', ' +
                    map_control.get_rid_value('raw_pct_in_seq_min')[2];
        }
        if (pct_min == pct_max) {
            total_sub = get_sample_size(total_global, sample_formula[3]);
            sample_track = total_sub;
            start_sub = map_control.get_rid_value(prefix + 'enroll_st_cn')[0];
            len_sub = len_global - start_sub + 1;
            tips1 = 'Global study - ' + total_global_label + ', ' + len_global_label;
            tips2 = 'Subpopulation (<font color="#ED5565"><b>n:' + sample_track + '/M:' + len_sub + '</b></font>) - ' +
                    tips2 + ', ' +
                    map_control.get_rid_value(prefix + 'enroll_st_cn')[2].replace('Subpopulation within global study:','');
            expected_list = set_expected_list(total_sub, len_sub);
            call_custom_recruitment_dialogue(r_id, total_sub, start_sub, tips1, tips2, expected_list, prefix + 'recruit_cn_in');
        } else {
            call_custom_recruitment_throw_dialogue(btn_id, r_id);
        }
    } else if (/^(ni_)*unif_noncn$/.test(r_id)) {
        // global (exclude local)
        // 4 Completely included - Global participants excluding subpopulation: Uniform recruitment?
        //   Partially included  - Global participants excluding subpopulation: Uniform recruitment?
        if (current_local_include[0] != 'part') {
            pct_min = map_control.get_rid_value('raw_pct_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_seq_max')[0];
            tips1 = '';
        } else {
            pct_min = map_control.get_rid_value('raw_pct_in_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_in_seq_max')[0];
            tips1 = ', ' + map_control.get_rid_value('raw_pct_in_seq_min')[2];
        }
        if (pct_min == pct_max) {
            total_sub = get_sample_size(total_global, sample_formula[1]);
            sample_track = total_sub;
            len_sub = len_global;
            tips1 = 'Global study - ' + total_global_label + ', ' + len_global_label + '</br>' +
                    'Subpopulation - ' + map_control.get_rid_value('raw_pct')[2] + tips1;
            tips2 = 'Global participants excluding subpopulation: <font color="#ED5565"><b>' + sample_track + '</b></font>';
            expected_list = set_expected_list(total_sub, len_sub);
            call_custom_recruitment_dialogue(r_id, total_sub, 1, tips1, tips2, expected_list, prefix + 'recruit_noncn');
        } else {
            call_custom_recruitment_throw_dialogue(btn_id, r_id);
        }
    } else if (/^(ni_)*unif_noncn_ex$/.test(r_id)) {
        // global_total
        // 5 Completely excluded - Global participants: Uniform recruitment?
        total_sub = total_global;
        len_sub = len_global;
        tips1 = total_global_label;
        tips2 = len_global_label;
        expected_list = set_expected_list(total_sub, len_sub);
        call_custom_recruitment_dialogue(r_id, total_sub, 1, tips1, tips2, expected_list, prefix + 'recruit_noncn_ex');
    } else if (/^(ni_)*unif_cn_out$/.test(r_id)) {
        // local (not in global)
        // 6 Completely excluded - Subpopulation out of global study: Uniform recruitment?
        //   Partially excluded  - Subpopulation out of global study: Uniform recruitment?
        //console.log(current_local_include[0]);
        if (current_local_include[0] != 'part') {
            pct_min = map_control.get_rid_value('raw_pct_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_seq_max')[0];
            tips2 = map_control.get_rid_value('raw_pct_seq_min')[2];
        } else {
            pct_min = map_control.get_rid_value('raw_pct_in_seq_min')[0];
            pct_max = map_control.get_rid_value('raw_pct_in_seq_max')[0];
            pct = map_control.get_rid_value('raw_pct')[0];
            tips2 = map_control.get_rid_value('raw_pct')[2] + ', ' +
                    map_control.get_rid_value('raw_pct_in_seq_min')[2];
        }
        if (pct_min == pct_max) {
            total_sub = get_sample_size(total_global, sample_formula[4]);
            sample_track = total_sub;
            len_sub = len_global;
            tips1 = 'Global study - ' + total_global_label;
            tips2 = 'Subpopulation (<font color="#ED5565"><b>n:' + sample_track + '/M:' + len_sub + '</b></font>) - ' +
                    tips2 + ', ' +
                    map_control.get_rid_value(prefix + 'enroll_len_out')[2].replace('Subpopulation out of global study: ','');
            expected_list = set_expected_list(total_sub, len_sub);
            call_custom_recruitment_dialogue(r_id, total_sub, 1, tips1, tips2, expected_list, prefix + 'recruit_cn_out');
        } else {
            call_custom_recruitment_throw_dialogue(btn_id, r_id);
        }
    }
}

function reset_ui_widget_init_value (r_id) {
    let info_arr = map_control.get_rid_value(r_id);
    const ui_widget = new Ui_Widget(info_arr[3], r_id, info_arr[4]);
    const ui_id = ui_widget.get_ui_id();
    ui_widget.set_ui_init_value(ui_id);
}

function call_custom_recruitment_dialogue (r_id, total, start, tips1, tips2, expected_list, list_var) {
    let rowNum = expected_list.length;
    //console.log(r_id + ', ' + list_var);
    const handsontable_settings = {
        type: 'numeric',
        allowEmpty: true,
        height: 280,
        rowHeaders: true,
        colWidths: 150,
        rowHeaderWidth: 100,
        maxCols: 1,
        maxRows: rowNum,
        colHeaders: ['Customized recruitment'],
        licenseKey: 'non-commercial-and-evaluation',
        contextMenu: ['cut', 'copy', '---------', 'undo', 'redo'],
        //
        afterGetRowHeader: function(row, TH) {
            let month = row + Number(start);
            TH.innerHTML = 'Month ' + month;
        }
    };

    const handsontable_total_settings = {
        rowHeaders: ['Available'],
        height: 30,
        colWidths: 150,
        rowHeaderWidth: 100,
        type: 'numeric',
        licenseKey: 'non-commercial-and-evaluation',
        cells(row, col) {
            const cellProperties = {};
            cellProperties.readOnly = true;
            const data = this.instance.getData();
            cellProperties.renderer = redRowRenderer;
            return cellProperties;
        }
    };
    //console.log('listen_ui_recruitment_button_open_dialog():');
    //console.log(tips1);
    //console.log(tips2);
    //console.log(expected_list);
    //
    let inner_table;
    let sum = 0;
    let tmp_arr;
    var modal = new tingle.modal({
        footer: true,
        stickyFooter: false,
        closeMethods: [],
        onOpen: function () {
  	        let total_container = document.getElementById('dialog_total');
  	        const inner_table_total = new Handsontable(total_container, handsontable_total_settings);
  	        inner_table_total.loadData([[total - sum]]);
  	        let table_container = document.getElementById('dialog_table');
            inner_table = new Handsontable(table_container, handsontable_settings);
            inner_table.updateSettings({
                afterChange: function (changes, src) {
                    sum = 0;
                    var values = inner_table.getDataAtCol(0);
                    for (let i = 0; i < values.length; i++) {
                        if (parseInt(values[i]).toString() != 'NaN') sum += parseInt(values[i]);
                    }
                    inner_table_total.loadData([[total - sum]]);
                    modal.setFooterContent('');
                    if (sum == total) {
                        modal.addFooterBtn("Don't Save", 'tingle-btn tingle-btn--pull-right', function() {
                            modal.close();
                            modal.destroy();
                            tmp_arr = map_control.get_rid_value(list_var);
                            //
                            if (tmp_arr[0][1] == 'NA') reset_ui_widget_init_value (r_id);
                            //
                            else map_control.set_rid_value(list_var, [tmp_arr[0][1], tmp_arr[0][1]]);
                        });
                        modal.addFooterBtn('Save', 'tingle-btn tingle-btn--primary tingle-btn--pull-right', function() {
                            tmp_arr = handsontable2array(inner_table);
                            map_control.set_rid_value(list_var, [tmp_arr, tmp_arr]);
                            modal.close();
                            modal.destroy();
                        });
                    } else {
                        modal.addFooterBtn("Don't Save", 'tingle-btn tingle-btn--pull-right', function() {
                            modal.close();
                            modal.destroy();
                            reset_ui_widget_init_value (r_id);
                        });
                    }
                }
            });
            let old_value = map_control.get_rid_value(list_var);
            old_value = old_value[0][1];
            //console.log(old_value);
            //console.log(expected_list);
            //
            if (old_value != 'NA') {
                if (old_value.length != expected_list.length) {
                    //
                    //
                    for (let i = 0; i < Math.min(old_value.length, expected_list.length); i++) {
                        expected_list[i] = old_value[i];
                    }
                    inner_table.loadData(array2handsontable(expected_list));
                } else {
                    //
                    inner_table.loadData(array2handsontable(old_value));
                }
            }
            //
            else inner_table.loadData(array2handsontable(expected_list));
        },
        onClose: function () {
            modal.destroy();
        }
    });
    //let header = r_id_labels.get(r_id).replace('?','');
    let header = r_id_labels.get(r_id);
    modal.setContent('<div><h3><b><font color=" #367FA9">' + header + '</font><font color=" #ED5565"> No</font></b></h3>' +
        '<h4>'+ current_local_include[2] + '</br></br>' + tips1 +'</br>' + tips2 + '</h4>' +
        '<div id="dialog_total"></div>' +
        '<div id="dialog_table"></div>' +
        '</div>');
    modal.open();
}

//
function call_custom_recruitment_throw_dialogue (btn_id, r_id) {
    let ui_name_pct_seq, r_id_pct_seq, slider, current_min_value, current_max_value;
    if (current_local_include[0] != 'part') {
        ui_name_pct_seq = 'ui_text_local_pct_seq';
        r_id_pct_seq = 'raw_pct_seq';
    } else {
        ui_name_pct_seq = 'ui_text_local_pct_in_seq';
        r_id_pct_seq = 'raw_pct_in_seq';
    }
    //console.log('call_custom_recruitment_throw_dialogue():');
    //console.log(ui_name_pct_seq);
    current_min_value = map_control.get_rid_value(r_id_pct_seq + '_min')[0];
    current_max_value = map_control.get_rid_value(r_id_pct_seq + '_max')[0];
    const modal = new tingle.modal({
        footer: true,
        closeMethods: [],
        onClose: function () {
            modal.destroy();
        }
    });
    const label = map_control.get_rid_value(r_id)[1];
    modal.setContent(
        '<div><h2>Note:</h2>' +
        "<h4>If '<font color='#367FA9'><b>" + label + "</b></font>" + 
        "' recruitment needs to be customized, please set minimum and maximum of '<font color='#367FA9'><b>" +
        map_control.get_rid_value(r_id_pct_seq)[1] +
        "'</b></font> to the same value.</h4>" +
        '<h4>Current min value is <font color="#ED5565"><b>' + current_min_value + 
        '</b></font>, max value is <font color="#ED5565"><b>' + current_max_value + 
        '</b></font></h4>' +
        '</div>'
    );
    modal.addFooterBtn('OK', 'tingle-btn tingle-btn tingle-btn--pull-right', function() {
        modal.close();
        modal.destroy();
        $('#ui_siderbar_box_local').removeClass('collapsed-box');
        $('#ui_siderbar_box_button_local').children('i').removeClass('fa fa-plus').addClass('fa fa-minus');
        reset_ui_widget_init_value (r_id);
    });
    modal.open();
}

//
function listen_ui_check_button (output_id, summary_table) {
    let ui_btn_id_str, output_name, btn_id;
    output_name = groupn_formats.get(output_id);
    ui_btn_id_str = 'ui_btn_' + output_name + '_check';
    btn_id = document.getElementById(ui_btn_id_str);
    btn_id.addEventListener("click", function() {
        //console.log(ui_btn_id_str + ' click!');    
        ui_input_change_flag = 0;
        if (ui_btn_id_str.substr(7, 6) != 'mcpmod') {
            //
            local_info.transloate_pct_seq();
        } else {
            // mcpmod only
            //console.log('mcpmod part only!');
            doselevel_control.push_data_to_map_control();
        }
        
        //
        const info_arr_track = map_control.transfer_to_output_track(output_name);
        let na_row_arr = get_na_row_id(info_arr_track, output_name);
        //console.log('info_arr_track:');
        //console.log(info_arr_track);
        //console.log(output_name);
        //console.log(na_row_arr);
        $('a[data-toggle="pill"][href="#tab-input_' + output_name + '-2"]').on('shown.bs.tab', function (e) {
  	        //console.log('tab-input_' + output_name + '-2: shown');
  	        summary_table.loadData(info_arr_track);
  	        const hiddenRowsPlugin = summary_table.getPlugin('hiddenRows');
            hiddenRowsPlugin.hideRows(na_row_arr);
            summary_table.render();
            //
            //
            let add_default_pattern = 'no';
            if (output_id == 3 || output_id == 6) {
                if (current_local_include[0] == 'no') add_default_pattern = 'yes';
            }
            let r_code_str = transfer_to_r_code(output_id, info_arr_track, add_default_pattern);
            //
            let textarea_id = document.getElementById('ui_textarea_' + output_name);
            textarea_id.value = r_code_str;
            textarea_id.dispatchEvent(new Event("change"));
            //
            //console.log(input_error_flag);
            if (input_error_flag == 1) {
                $("#ui_btn_" + output_name + "_run").attr('disabled', true);
            } else {
                $("#ui_btn_" + output_name + "_run").attr('disabled', false);
            }
            //
            //$('#global_info').attr('disabled', true);
            //$('#local_info').attr('disabled', true);
            //$("#ui_text_local_pct_seq").data("ionRangeSlider").update({block: true});
            //$("#ui_text_local_pct_in_seq").data("ionRangeSlider").update({block: true});
            //$('#simulation_info').attr('disabled', true);
        });
    })
}

//
function listen_ui_refresh_button (output_id) {
    let ui_btn_id_str, output_name, btn_id;
    output_name = groupn_formats.get(output_id);
    ui_btn_id_str = 'ui_btn_' + output_name + '_refresh';
    btn_id = document.getElementById(ui_btn_id_str);
    btn_id.addEventListener("click", function() {
        //console.log(ui_btn_id_str + ' click!');
        //console.log('#shiny-tab-' + output_name + ' .nav.nav-pills a:first');
        ui_input_change_flag = 0;
        $('#shiny-tab-' + output_name + ' .nav.nav-pills a:first').tab('show');
        $('#ui_btn_' + output_name + '_check').trigger('click');
    })
}

//
function listen_ui_download_button (output_id, summary_table) {
    let ui_btn_id_str, output_name, btn_id;
    output_name = groupn_formats.get(output_id);
    ui_btn_id_str = 'ui_btn_' + output_name + '_download';
    if (output_name == 't2e_end_life_test') output_name = 't2e_end_logrank_test';
    //
    //let now = new Date().format("Ymd_hi");
    let now = new Date().format("Ymd");
    btn_id = document.getElementById(ui_btn_id_str);
    //console.log('ui_btn_' + output_name + '_download: clicked');
    const exportPlugin = summary_table.getPlugin('exportFile');
    btn_id.addEventListener('click', () => {
        exportPlugin.downloadFile('csv', {
            bom: false,
            columnDelimiter: ',',
            columnHeaders: true,
            exportHiddenColumns: true,
            exportHiddenRows: false,
            fileExtension: 'csv',
            filename: 'RegionSizeR_' + output_name + '_' + now,
            mimeType: 'text/csv',
            rowDelimiter: '\r\n',
            rowHeaders: true
        });
    });
}

function transfer_to_r_code (out_id, arr, add_default_pattern) {
    let str;
    if (out_id == 7 || out_id == 8) {
        //
        str = '### models ###\n';
        let n_model = 0;
        arr.forEach (item => {
            //console.log(item);
            let key = item[1], value = item[6];
            if (key.substr(0, 5) == 'model' && value != 'NA') {
                n_model++;
            } else if (key == 'mods_str') {
                str = str + value + '\n';
            }
        });
        //
        str = str + '\n### result_df ###\n';
        str = str + 'result_df(\n\t  fmodels = models\n';
        //
        //str = str + '\t, n_model = ' + n_model + '\n';
        let e0;
        arr.forEach (item => {
            let key = item[1], type = item[5], value = item[6];
            if (key == 'e0') e0 = value;
            if (key.substr(0, 5) != 'model' && key != 'e0' && key != 'emax' && key != 'mods_str') {
                if (value != 'NA') {
                    if (type == 'string') str = str + '\t, ' + key + ' = "' + value + '"\n';
                    else {
                        //
                        if (key == 'nsim') str = str + '\t, iter = ' + value + '\n';
                        else if (key == 'delta') {
                            if (out_id == 7) str = str + '\t, ' + key + ' = ' + value + '\n';
                            else if (out_id == 8) str = str + '\t, ' + key + ' = abs(logit(' + value + ') - logit(' + e0 + '))\n';
                        }
                        else str = str + '\t, ' + key + ' = ' + value + '\n';
                    }
                }
            }
        });
        str = str + '\t, n_model = ' + n_model + '\n';
        str = str + '\t)';
        //result_df_bin (doses=doses_, fmodels=fmodels_, n_arm=n_arm_, alpha=alpha, type=type, pi=pi, n_model=n_model, iter=10000, direction='increasing', n_arm_reg = n_arm_reg_)
        
    } else {
        str = 'plotSim (\n';
        let i = 1;
        arr.forEach (item => {
            if (item[6] != 'NA') {
                if (i == 1) str = str + '\t  ';
                else str = str + '\t, ';
                if (item[5] == 'string') str = str + item[1] + ' = "' + item[6] + '"\n';
                else str = str + item[1] + ' = ' + item[6] + '\n';
                i++;
            }
        });
        //
        if (add_default_pattern == 'yes') str = str + '\t, pattern = 0\n';
        str = str + ')';
    }
    //console.log('transfer_to_r_code():');
    //console.log(str);
    return str;
}

function get_na_row_id (data) {
    //console.log('get_na_row_id():');
    let out = [], row = 0;
    data.forEach (item => {
        //console.log(item);
        if (item[6] == 'NA' || item[1] == 'mods_str') out.push(row);
        row++;
    });
    //console.log(out);
    return out;
}

function array2handsontable(arr) {
    let arrLen = arr.length;
    let out_2d_array = [];
    for (let i = 0; i < arrLen; i++) {
        out_2d_array.push([arr[i]]);
    }
    //console.log('array2handsontable():');
    //console.log(out_2d_array);
    return out_2d_array;
}

function handsontable2array(table) {
    let arr = table.getData();
    let arrLen = arr.length;
    let out_1d_array = [];
    for (let i = 0; i < arrLen; i++) {
        if (arr[i][0] == '') out_1d_array.push(0);
        else out_1d_array.push(arr[i][0]);
    }
    //console.log('handsontable2array():');
    //console.log(out_1d_array);
    return out_1d_array;
}

//
async function main () {
    // Init
    //
    btn_show_ui_tabpane();

    // Check Button click
    // 1 Superiority > Continuous Endpoint >> T test
    const summary_table1 = create_summary_table(1);
    listen_ui_check_button(1, summary_table1);
    listen_ui_refresh_button(1);
    listen_ui_download_button(1, summary_table1);
    
    // 2 Superiority > Binary Endpoint >> Chi-square
    const summary_table2 = create_summary_table(2);
    listen_ui_check_button(2, summary_table2);
    listen_ui_refresh_button(2);
    listen_ui_download_button(2, summary_table2);
    
    // 3 Superiority > Time-to-event Endpoint >> Life test
    const summary_table3 = create_summary_table(3);
    listen_ui_check_button(3, summary_table3);
    listen_ui_refresh_button(3);
    listen_ui_download_button(3, summary_table3);

    // 4 Non-inferiority > Continuous Endpoint
    const summary_table4 = create_summary_table(4);
    listen_ui_check_button(4, summary_table4);
    listen_ui_refresh_button(4);
    listen_ui_download_button(4, summary_table4);
    
    // 5 Non-inferiority > Binary Endpoint
    const summary_table5 = create_summary_table(5);
    listen_ui_check_button(5, summary_table5);
    listen_ui_refresh_button(5);
    listen_ui_download_button(5, summary_table5);
    
    // 6 Non-inferiority > Time-to-event Endpoint
    const summary_table6 = create_summary_table(6);
    listen_ui_check_button(6, summary_table6);
    listen_ui_refresh_button(6);
    listen_ui_download_button(6, summary_table6);
    
    // 7 MCP-Mod > Continuous Endpoint
    const summary_table7 = create_summary_table(7);
    listen_ui_check_button(7, summary_table7);
    listen_ui_refresh_button(7);
    listen_ui_download_button(7, summary_table7);
    
    // 8 MCP-Mod > Binary Endpoint
    const summary_table8 = create_summary_table(8);
    listen_ui_check_button(8, summary_table8);
    listen_ui_refresh_button(8);
    listen_ui_download_button(8, summary_table8);
    
    listing_ui_reset_all();
    
    listing_upload_old_csv_file();
    
    if (status == 'prod') {
        $('.uiKeyArea').hide();
    }
    
    const $l_container = document.querySelector('#ui_table_mcpmod_local_doselevel');
    local_doselevel_table = new SimpleDataTable($l_container, {
        withRemoveButton: false,
        withAddButton: false,
        keyColumnReadonly: 2,
        defaultColumnNumber: 3,
        colWidth: [8, 35, 57]
    });
    local_doselevel_table.setHeaders(['ID', 'Global patients', 'Subpopulation patients']);
    local_doselevel_table.render();
    
    const $g_container = document.querySelector('#ui_table_mcpmod_global_doselevel');
    global_doselevel_table = new SimpleDataTable($g_container, {
        defaultColumnNumber: 3,
        colWidth: [8, 52, 36, 5],
        maxRowCount: maxGroupCount
    });
    global_doselevel_table.setHeaders(['ID', 'Dosage in group', 'Number of patients']);
    global_doselevel_table.on(SimpleDataTable.EVENTS.UPDATE, (data) => {
        //console.log('Global_doselevel_table.ON.UPDATE Event!');
        //console.log(data);
        //
        doselevel_control.set_value_from_global_table(data);
        var mcpmod_dose_level = doselevel_control.get_value_for_local_table();
        //
        const totaln_id1 = document.getElementById('ui_label_mcpmod_global_totaln');
        totaln_id1.innerHTML='Total sample size: <font color="#ED5565"><b>' + 
                            doselevel_control.global_totaln + '</b></font>';
        const totaln_id2 = document.getElementById('ui_label_mcpmod_global_totaln_clone');
        totaln_id2.innerHTML='Total sample size: <font color="#ED5565"><b>' + 
                            doselevel_control.global_totaln + '</b></font>';
        const totaln_id3 = document.getElementById('ui_label_mcpmod_local_totaln');
        totaln_id3.innerHTML='Total sample size: <font color="#ED5565"><b>' + 
                            doselevel_control.local_totaln + '</b></font>';
        local_doselevel_table.load(mcpmod_dose_level);
        local_doselevel_table.render();
    });
    global_doselevel_table.load([
        {column1: 1, column2: null, column3: null}
    ]);
    global_doselevel_table.render();
    //
    global_doselevel_table.on(SimpleDataTable.EVENTS.ROW_REMOVED, index => {
        doselevel_control.remove_row(index);
    });
    //
    local_doselevel_table.on(SimpleDataTable.EVENTS.UPDATE, (data) => {
        doselevel_control.set_value_from_local_table(data);
        //
        const totaln_id = document.getElementById('ui_label_mcpmod_local_totaln');
        totaln_id.innerHTML='Total sample size: <font color="#ED5565"><b>' + 
                            doselevel_control.local_totaln + '</b></font>';
    });
    
    
    //
    const $con_model_container = document.querySelector('#mcpmod_con_end_candidate_model_table');
    con_model_table = new SimpleModelDataTable($con_model_container, {
        withAddButton: true,
        withRemoveButton: true,
        colWidth: [5, 20, 18, 18, 18, 18],
        maxRowCount: maxModelCount,
        target: 'con_mcpmod'
    });
    con_model_table.setHeaders(['ID', 'Model type', 'Parameters', '', '', '']);
    con_model_table.load([
        {column1: 1,
         column2: '',
         column3: '',
         column4: null,
         column5: '',
         column6: null
        }
    ]);
    con_model_table.render();
    //
    con_model_table.on(SimpleModelDataTable.EVENTS.UPDATE, (data) => {
        let current_data = con_model_control.get_value_from_model_table_compare_update_control(data);
        con_model_table.load(current_data);
        con_model_table.render();
    });
    //
    con_model_table.on(SimpleDataTable.EVENTS.ROW_REMOVED, index => {
        con_model_control.remove_row(index);
    });
    
    //
    const $bin_model_container = document.querySelector('#mcpmod_bin_end_candidate_model_table');
    bin_model_table = new SimpleModelDataTable($bin_model_container, {
        withAddButton: true,
        withRemoveButton: true,
        colWidth: [5, 20, 18, 18, 18, 18],
        maxRowCount: maxModelCount,
        target: 'bin_mcpmod'
    });
    bin_model_table.setHeaders(['ID', 'Model type', 'Parameters', '', '', '']);
    bin_model_table.load([
        {column1: 1,
         column2: '',
         column3: '',
         column4: null,
         column5: '',
         column6: null
        }
    ]);
    bin_model_table.render();
    //
    bin_model_table.on(SimpleModelDataTable.EVENTS.UPDATE, (data) => {
        let current_data = bin_model_control.get_value_from_model_table_compare_update_control(data);
        bin_model_table.load(current_data);
        bin_model_table.render();
    });
    //
    bin_model_table.on(SimpleDataTable.EVENTS.ROW_REMOVED, index => {
        bin_model_control.remove_row(index);
    });
    
    //
    map_control.set_rid_value('totaln_mcpmod', 0);
    map_control.set_rid_value('cn_mcpmod', 0);
    //
    ui_init(ui_map_control);
    //console.log(map_control);
}

function reset_ui_use_default_value (items = []) {
    items.forEach(item => {
        if (item.ui_id != null) {
            const ui_widget = new Ui_Widget(item.ui_id, item.r_id, item.init);
            const id = ui_widget.get_ui_id();
            ui_widget.set_ui_init_value(id);
        } else {
            if (/^(ni_)*recruit_/.test(item.r_id)) {
                map_control.set_rid_value (item.r_id, [undefined, 'NA']);
            }
            mcpmod_use_default_value_fill_ui(item.r_id);
        } 
    });
}

function mcpmod_use_default_value_fill_ui (r_id, jsonString=null) {
    if (r_id == 'global_doses') {
        //
        if (jsonString == null) {
            doselevel_control.set_values_from_history_arrstr_to_doselevel_control(doselevel_control_default_value);
        } else {
            doselevel_control.set_values_from_history_arrstr_to_doselevel_control(jsonString);
        }
        //
        let ltable = doselevel_control.get_values_from_doselevel_control_to_local_table();
        local_doselevel_table.load(ltable);
        local_doselevel_table.render();
        //
        let gtable = doselevel_control.get_values_from_doselevel_control_to_global_table();
        global_doselevel_table.load(gtable);
        global_doselevel_table.render();
    } else if (r_id == 'con_model1') {
        //
        if (jsonString == null) {
            con_model_control.set_values_from_history_arrstr_to_model_control(con_end_model_default_value);
            //
            con_model_table.load(JSON.parse(con_end_model_default_value));
        } else {
            con_model_control.set_values_from_history_arrstr_to_model_control(jsonString);
            con_model_table.load(JSON.parse(jsonString));
        }
        con_model_table.render();
    } else if (r_id == 'bin_model1') {
        //
        if (jsonString == null) {
            bin_model_control.set_values_from_history_arrstr_to_model_control(bin_end_model_default_value);
            bin_model_table.load(JSON.parse(bin_end_model_default_value));
        } else {
            bin_model_control.set_values_from_history_arrstr_to_model_control(jsonString);
            bin_model_table.load(JSON.parse(jsonString));
        }
        bin_model_table.render();
    }
}

// Init
//
function ui_init (items = []) {
        
    listing_ui_chinese_all_in_show_left_test (key = 'tte');
    listing_ui_chinese_all_in_show_left_test (key = 'tte_ni');
    listing_ui_pattern_show_left_test (key = 'tte');
    listing_ui_pattern_show_left_test (key = 'tte_ni');
        
    items.forEach(item => {
        if (item.ui_id != null) {
            const ui_widget = new Ui_Widget(item.ui_id, item.r_id, item.init);
            const id = ui_widget.get_ui_id();
            //
            ui_widget.listen_ui_click(id);
            //
            ui_widget.set_ui_init_value(id);
        } else {
            mcpmod_use_default_value_fill_ui(item.r_id);
        }
        
    });

    listing_ui_dropdown_show_partial_fieldset();
    listing_ui_mcpmod_delta_na_change_value('con_end');
    listing_ui_mcpmod_delta_na_change_value('bin_end');
        
    //
    $(function () {
        const ui_treeview_num = $('.treeview').length;
        var i = 1;
        while (i <= ui_treeview_num) {
            //
            //
            listing_ui_treeview_show_ni_fieldset(i);
            listing_ui_treeview_show_mcpmod_fieldset(i);
            //
            listing_ui_treeview_menu_show_tab1(i);
            i++;
        }
    });
    
    //
    $(function () {
        const ui_tabpane_num = $('.nav.nav-pills').length;
        var i = 1;
        while (i <= ui_tabpane_num) {
            //
            //listening_ui_tabpane_update(i);
            $('#tabset' + i + ' a:first').tab('show');
            $('#tabset' + i + ' li:eq(1) a').addClass('hide');
            $('#tabset' + i + ' li:eq(2) a').addClass('hide');
            i++;
        }
    });
    
}

//
function listing_ui_treeview_show_ni_fieldset (treeview_id) {
    $(function () {
        $("#treeview" + treeview_id + " > a").on('click', function(e) {
            if ($(this).parent().attr("id") == "treeview2") {
                $("#ui_fieldset_ni_nim_hma").css('display','block');
            } else {
                $("#ui_fieldset_ni_nim_hma").css('display','none');
            }
        });
    });
}

//
function listing_ui_treeview_show_mcpmod_fieldset (treeview_id) {
    $(function () {
        $("#treeview" + treeview_id + " > a").on('click', function(e) {
            if ($(this).parent().attr("id") == "treeview3") {
                $("#ui_siderbar_mcpmod_global").css('display','block');
                $("#ui_siderbar_mcpmod_local").css('display','block');
                $("#ui_siderbar_global").css('display','none');
                $("#ui_siderbar_local").css('display','none');
            } else {
                $("#ui_siderbar_mcpmod_global").css('display','none');
                $("#ui_siderbar_mcpmod_local").css('display','none');
                $("#ui_siderbar_global").css('display','block');
                $("#ui_siderbar_local").css('display','block');
            }
        });
    });
}

//
function listing_ui_chinese_all_in_show_left_test (key='tte') {
    $("#ui_select_local_include").on('change', function(e) {
        let pattern, prefix;
        current_local_include[0] = $(this).find('option:selected').val();
        current_local_include[1] = $(this).find('option:selected').text();
        current_local_include[2] = 'Subpopulation <font color="#ED5565"><b>'+ current_local_include[1]
                                   + '</b></font> in the global';
        //console.log('listing_ui_chinese_all_in_show_left_test(): current_local_include:');
        //console.log(current_local_include);
        //console.log('listing_ui_chinese_all_in_show_left_test():监听ui_select_local_include的change事件');
        $('[name="ui_legend_of_chinese_all_in"]').text('');
        $('[name="ui_legend_of_chinese_all_in"]').append(current_local_include[2]);
        if (key == 'tte') {
            pattern = document.getElementById('ui_number_life_test_pattern');
            prefix = '';
        } else if (key == 'tte_ni') {
            pattern = document.getElementById('ui_number_ni_t2e_end_pattern');
            prefix = 'ni_'
        }
        if (current_local_include[0] == 'yes') {
            // yes: 1, 2(pattern=yes), 3, 4(pattern=no)
            $("#" + prefix + "fieldset_pattern").css('display','block');
            $("#" + prefix + "fieldset_global_study").css('display','block');
            $("#" + prefix + "fieldset_local_in_global").css('display','block');
            $("#" + prefix + "fieldset_local_out_global").css('display','none');
            $("#" + prefix + "fieldset_ex_global").css('display','none');
            $("#" + prefix + "fieldset_ex_local").css('display','none');
            pattern.dispatchEvent(new Event("change"));
        } else if (current_local_include[0] == 'part') {
            // part: 1, 2(pattern=yes), 3(pattern=no), 4(pattern=no), 6
            $("#" + prefix + "fieldset_pattern").css('display','block');
            $("#" + prefix + "fieldset_global_study").css('display','block');
            $("#" + prefix + "fieldset_local_in_global").css('display','block');
            $("#" + prefix + "fieldset_local_out_global").css('display','none');
            $("#" + prefix + "fieldset_ex_global").css('display','none');
            $("#" + prefix + "fieldset_ex_local").css('display','block');
            pattern.dispatchEvent(new Event("change"));
        } else if (current_local_include[0] == 'no') {
            // no: 5, 6
            $("#" + prefix + "fieldset_pattern").css('display','none');
            $("#" + prefix + "fieldset_global_study").css('display','none');
            $("#" + prefix + "fieldset_local_in_global").css('display','none');
            $("#" + prefix + "fieldset_local_out_global").css('display','none');
            $("#" + prefix + "fieldset_ex_global").css('display','block');
            $("#" + prefix + "fieldset_ex_local").css('display','block');
        }
    });
}

//
function listing_ui_pattern_show_left_test (key='tte') {
    let ui_id, prefix;
    if (key == 'tte') {
        ui_id = 'ui_number_life_test_pattern';
        prefix = '';
    } else if (key == 'tte_ni') {
        ui_id = 'ui_number_ni_t2e_end_pattern';
        prefix = 'ni_';
    }
    $('#' + ui_id).on('change', function(e) {
        //
        if ($(this).attr('value') == 1) {
            $('#' + prefix + "fieldset_global_study").css('display','block');
            $('#' + prefix + "fieldset_local_out_global").css('display','none');
            $('#' + prefix + "input_recru_local_in_global").css('display','none');
            current_pattern[0] = 'Y';
            current_pattern[1] = 'Pattern: Subpopulation within global study has the <font color="#ED5565"><b>same</b></font> recruitment pattern with global study';
        } else {
            $('#' + prefix + "fieldset_global_study").css('display','none');
            $('#' + prefix + "fieldset_local_out_global").css('display','block');
            $('#' + prefix + "input_recru_local_in_global").css('display','block');
            current_pattern[0] = 'N';
            current_pattern[1] = 'Pattern: Subpopulation within global study has the <font color="#ED5565"><b>different</b></font> recruitment pattern with global study';
        }
        //console.log('listing_ui_pattern_show_left_test():');
        //console.log(current_pattern);
    });
}

//
//
function listing_ui_treeview_menu_show_tab1 (treeview_id) {
    $(function () {
        $("#treeview" + treeview_id + " .treeview-menu > li > a").on('click', function(e) {
            const current_tab_href = $(this).attr("href");
            if (current_tab_href != '#') { 
                //
                $(current_tab_href + ' .nav.nav-pills a:first').tab('show');
                $(current_tab_href + ' .nav.nav-pills li:eq(1) a').addClass('hide');
                $(current_tab_href + ' .nav.nav-pills li:eq(2) a').addClass('hide');
                //
                ui_input_change_flag = 0;
                //
                //$('#global_info').attr('disabled', false);
                //$('#local_info').attr('disabled', false);
                //("#ui_text_local_pct_seq").data("ionRangeSlider").update({block: false});
                //$("#ui_text_local_pct_in_seq").data("ionRangeSlider").update({block: false});
                //$('#simulation_info').attr('disabled', false);
            }
        });
    });
}

//
function listing_ui_dropdown_show_partial_fieldset () {
    $(function () {
        $('#ui_select_local_include').on('change', function(){
            const current_value = $(this).find('option:selected').text();
            //console.log(current_value);
            if (current_value != 'partially included') {
                $("#ui_fieldset_no_partial").css('display','block');
                $("#ui_fieldset_partial").css('display','none');
            } else {
                $("#ui_fieldset_partial").css('display','block');
                $("#ui_fieldset_no_partial").css('display','none');
            }
            //
            $('.uiLabel').css('color','black');
        });
    });
}

//
//
function btn_show_ui_tabpane () {
    $(function () {
        $(".uiCheckBtn").on('click', function(e) {
            //console.log(this.id);
            const current_tab_id = $(this).parents('.tab-pane').attr('id');
            //console.log(current_tab_id);
            const next_tab_id = $('#' + current_tab_id).next().attr('id');
            //console.log(next_tab_id);
            $('a[href*="#' + next_tab_id + '"]').removeClass('hide');
            $('a[href*="#' + next_tab_id + '"]').tab('show');
        });
    });
}

//
//
function listening_ui_tabpane_update (tabpane_id) {
    $(function () {
        $("#tabset" + tabpane_id + ".nav.nav-pills > li > a").on('click', function(e) {
            const current_tab_id = $(this).parent().index();
            if (current_tab_id !=1) {
                let i = current_tab_id;
                while (i < 2) {
                    $('#tabset' + tabpane_id +' li:eq(' + (i + 1) + ') a').addClass('hide');
                    i++;
                }
                if (current_tab_id == 0) {
                    //
                    //$('#global_info').attr('disabled', false);
                    //$('#local_info').attr('disabled', false);
                    //$("#ui_text_local_pct_seq").data("ionRangeSlider").update({block: false});
                    //$("#ui_text_local_pct_in_seq").data("ionRangeSlider").update({block: false});
                    //$('#simulation_info').attr('disabled', false);
                }
            }
        });
    });
}

//
function listing_ui_reset_all () {
    $('#ui_btn_default_reset').on('click', function(e) {
        reset_ui_use_default_value(ui_map_control);
        $('a[href*=\"#shiny-tab-blank\"]').tab('show');
        call_reset_dialog ('default');
    });
    $('#ui_btn_history_reset').on('click', function(e) {
        let r_id, ui_id, tab_id;
        hist_value_control.data.forEach(item => {
            r_id = item.r_id;
            if (r_id != null) {
                r_id = item.r_id.replace(/std_/gi,'raw_');
                ui_id = r_id_ui_ids.get(r_id);
                //console.log(r_id + ', ' + ui_id + ', ' + item.value_raw + ', ' + item.group);
                if (ui_id != null) {
                    const ui_widget = new Ui_Widget(ui_id, r_id, item.value_raw);
                    const id = ui_widget.get_ui_id();
                    ui_widget.set_ui_init_value(id);
                    if (r_id == 'con_delta'){
                        const ui_widget = new Ui_Widget(ui_id + '_yes', r_id + '_yes', (isNaN(item.value_raw)) ? 0:1);
                        const id = ui_widget.get_ui_id();
                        ui_widget.set_ui_init_value(id);
                    }
                } else {
                    if (/^(ni_)*recruit_/.test(r_id)) {
                        let tmp = item.value_raw.split(',').map(Number);
                        //console.log(tmp);
                        map_control.set_rid_value (r_id, [tmp, tmp]);
                        let x=map_control.get_rid_value (r_id);
                        //console.log(x);
                    }
                    mcpmod_use_default_value_fill_ui(r_id, item.value_raw);
                }
                tab_id = item.group;
                //console.log(tab_id);
            }
        });
        //
        $('#tab_' + tab_id).trigger('click');
        //
        if (/^ni_/.test(tab_id)) {
            $("#ui_fieldset_ni_nim_hma").css('display','block');
        } else {
            $("#ui_fieldset_ni_nim_hma").css('display','none');
        }
        if (/^mcpmod_/.test(tab_id)) {
            $("#ui_siderbar_mcpmod_global").css('display','block');
            $("#ui_siderbar_mcpmod_local").css('display','block');
            $("#ui_siderbar_global").css('display','none');
            $("#ui_siderbar_local").css('display','none');
        } else {
            $("#ui_siderbar_mcpmod_global").css('display','none');
            $("#ui_siderbar_mcpmod_local").css('display','none');
            $("#ui_siderbar_global").css('display','block');
            $("#ui_siderbar_local").css('display','block');
        }
        //
        call_reset_dialog ('history',group_formats.get(tab_id));
    });
}

//
function call_reset_dialog (type, desc='') {
    //
    var modal = new tingle.modal({
        footer: true,
        closeMethods: [],
        onClose: function () {
            modal.destroy();
        }
    });
    //console.log('ok dialogue');
    if (type == 'default') {
        modal.setContent(
            '<div><h3>Use default values reset input</h3>' +
            '<h4>Reset was executed successfully!</h4>' +
            '</div>'
        );
    } else if (type == 'history') {
        modal.setContent(
            '<div><h3>Use historical values reset input</h3>' +
            '<h4><font color="#ED5565"><b>' + hist_file_name + '</b></font></h4>' +
            '<h4>Page <font color="#ED5565"><b>' + desc +'</b></font> has been loaded successfully!</h4>' +
            '</div>'
        );
    }
    modal.addFooterBtn('OK', 'tingle-btn tingle-btn--default tingle-btn--pull-right', function () {
        modal.close();
    });
    modal.open();
}

//
function create_summary_table (track_id) {
    // 
    //console.log('call_summary_table():');
    const container = document.getElementById('summary_track_table' + track_id);
    let merge_data, merge_data_dect;
    if (track_id < 7) {
        merge_data = [
            {row:  0, col: 3, rowspan: 6, colspan: 1},
            {row:  6, col: 3, rowspan: 6, colspan: 1},
            {row: 12, col: 3, rowspan: 2, colspan: 1}
        ];
    } else {
        merge_data = [
            {row: 0, col: 3, rowspan: 3, colspan: 1},
            {row: 3, col: 3, rowspan: 3, colspan: 1},
            {row: 6, col: 3, rowspan: 2, colspan: 1}
        ];
    }
    if (track_id == 1 || track_id == 4) {
        merge_data_dect = [{row: 14, col: 3, rowspan: 4, colspan: 1}];
    } else if (track_id == 2 || track_id == 5) {
        merge_data_dect = [{row: 14, col: 3, rowspan: 2, colspan: 1}];
    } else if (track_id == 3 || track_id == 6) {
        merge_data_dect = [{row: 14, col: 3, rowspan: 23, colspan: 1}];
    } else if (track_id == 7 || track_id == 8) {
        //mcpmod tbd
        merge_data_dect = [{row: 8, col: 3, rowspan: 16, colspan: 1}];
    }
    merge_data = merge_data.concat(merge_data_dect);
    const hot = new Handsontable(container, {
        data: Handsontable.helper.createSpreadsheetData(50, 5),
        readOnly: true,
        colWidths: [100, 100, 100, 110, 300, 100, 100, 290, 100],
        rowHeaders: false,
        colHeaders: ['r_id', 'r_id_alias', 'group', 'Category', 'Paramater Label',
                     'value_type', 'value_r', 'Parameter Value', 'value_raw'],
        height: 450,
        licenseKey: 'non-commercial-and-evaluation',
        hiddenColumns: {
            columns: [0, 1, 2, 5, 6, 8]
        },
        mergeCells: merge_data,
        hiddenRows: {
            copyPasteEnabled: true,
            indicators: true
        },
        cells(row, col) {
            const cellProperties = {};
            cellProperties.readOnly = true;
            const data = this.instance.getData();
            cellProperties.renderer = errorRenderer;
            return cellProperties;
        }
    });
    return hot;
}

function listing_upload_old_csv_file () {
    $('input[type="file"]').change(function(e){
        var file = e.target.files[0];
        hist_file_name = file.name;
        if (file.size > 0) {
            $("#ui_btn_history_reset").css('display','block');
        }
        Papa.parse(file, {
            header: true,
            dynamicTyping: true,
            complete: function(results) {
                hist_value_control = results;
                //console.log('hist_value_control:');
                //console.log(hist_value_control);
            },
            error: function(err, file) {
			    //console.log("ERROR:", err, file);
				firstError = firstError || err;
				errorCount++;
			},
        });
    });
			
}


//
function listing_ui_mcpmod_delta_na_change_value (key) {
    let ui_id = '#ui_number_mcpmod_' + key + '_delta_yes';
    $(ui_id).on('change', function(e) {
        //
        //
        if ($(this).is(':checked') == true) {
            $('#ui_number_mcpmod_' + key + '_delta').removeAttr("readonly");
            $('#ui_select_mcpmod_' + key + '_type').removeAttr("disabled");
        } else {
            $('#ui_number_mcpmod_' + key + '_delta').val('0');
            $('#ui_number_mcpmod_' + key + '_delta').attr("readonly","readonly");
            $('#ui_select_mcpmod_' + key + '_type').val('1');
            $('#ui_select_mcpmod_' + key + '_type').attr("disabled","disabled");
            map_control.set_rid_value(key.replace(/_end/, '_delta'), 0);
            map_control.set_rid_value(key.replace(/_end/, '_type'), 1);
        }
    });
}

function openUserTips() {
    window.open("RegionSizeR_User_Tips.docx", "_blank");
}
