/**
 * jspsych-image-button-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["threemines-pred"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'threemines-pred',
    description: '',
    parameters: {
      choices: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Choices',
        default: undefined,
        array: true,
        description: 'The labels for the buttons.'
      },
      button_html: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button HTML',
        default: '<button class="jspsych-btn">%choice%</button>',
        array: true,
        description: 'The html of the button. Can create own style.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed under the button.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial.'
      },
      margin_vertical: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin vertical',
        default: '0px',
        description: 'The vertical margin of the button.'
      },
      margin_horizontal: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin horizontal',
        default: '8px',
        description: 'The horizontal margin of the button.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, then trial will end when user responds.'
      },
      block_id: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Condition',
        default: null,
        description: 'Which condition is the participant in.'
      },
      bonus_phase: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Bonus amount and phase',
        default: "",
        description: 'Text to display for bonus and phase (for observation and practice).'
      },
      bonus_amount: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: 'Bonus amount variable',
        default: 0,
        description: 'Bonus amount to increment in test phase.'
      },
      stimuli: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Image stimuli',
        default: null,
        array: true,
        description: 'Source for images to display.'
      },
      choice_seq:{
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial choice',
        default: null,
        description: 'Fixed choice for this trial.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {


        

        ////// DISPLAY BONUS and PHASE
        var html = trial.bonus_phase;
    


        if (trial.choice_seq == 1){
            var side = "left"
             html += '<img src="'+trial.stimuli[0].sel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[1].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[2].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } else if (trial.choice_seq == 2){
            var side = "center"
             html += '<img src="'+trial.stimuli[0].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[1].sel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[2].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } else if (trial.choice_seq == 3) {
            var side = "right"
             html += '<img src="'+trial.stimuli[0].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[1].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[2].sel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } 




//show prompt if there is one
if (trial.prompt !== "") {
    html += trial.prompt;
} 



    //display buttons
    var buttons = [];
    if (Array.isArray(trial.button_html)) {
      if (trial.button_html.length == trial.choices.length) {
        buttons = trial.button_html;
      } else {
        console.error('Error in image-button-response plugin. The length of the button_html array does not equal the length of the choices array');
      }
    } else {
      for (var i = 0; i < trial.choices.length; i++) {
        buttons.push(trial.button_html);
      }
    }
    html += '<div id="jspsych-image-button-response-btngroup">';

    for (var i = 0; i < trial.choices.length; i++) {
      var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
      html += '<div class="jspsych-image-button-response-button" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'" id="jspsych-image-button-response-button-' + i +'" data-choice="'+i+'">'+str+'</div>';
    }
    html += '</div>';


    display_element.innerHTML = html;



    // start timing
    var start_time = performance.now();

    for (var i = 0; i < trial.choices.length; i++) {
      display_element.querySelector('#jspsych-image-button-response-button-' + i).addEventListener('click', function(e){
        var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
        after_response(choice);
      });
    }



    // store response
    var response = {
      rt: null,
      button: null
    };

    // function to handle responses by the subject
    function after_response(choice) {


      // measure rt
      var end_time = performance.now();
      var rt = end_time - start_time;
      response.button = choice;
      response.rt = rt;


        //determine choice side for output
        if (choice == 0){
            if(counterbalance_order == "CopperFirst") {
              pred_chosen = "copper"
            } else {
              pred_chosen = "iron"
            }
          } else {
            if(counterbalance_order == "IronFirst") {
              pred_chosen = "copper"
            } else {
              pred_chosen = "iron"
            }      
          }


      // disable all the buttons after a response
      var btns = document.querySelectorAll('.jspsych-image-button-response-button button');
      for(var i=0; i<btns.length; i++){
        //btns[i].removeEventListener('click');
        btns[i].setAttribute('disabled', 'disabled');
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // function to end trial when it is time
    function end_trial() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        "rt": response.rt,
        "block_id": trial.block_id,
        "pred_chosen": pred_chosen,
        "mine_chosen": trial.choice_seq
      };


      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };



    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-image-button-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if time limit is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();
