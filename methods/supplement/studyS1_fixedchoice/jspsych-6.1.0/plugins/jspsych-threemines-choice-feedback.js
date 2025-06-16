/**
 * jspsych-image-button-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["threemines-choice-feedback"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'threemines-choice-feedback',
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
        default: "",
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
      weights: {
        type: jsPsych.plugins.parameterType.FLOAT,
        pretty_name: 'Mine weights',
        default: null,
        array: true,
        description: 'True value of each mine.'
      },
      latent_intervention: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Latent Intervention',
        default: null,
        description: 'Whether or not an intervention occurs on this trial.'
      },
      reveals: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Reveal cause',
        default: null,
        description: 'Whether or not the cause is revealed on this trial.'
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
      }
    }
  }

  plugin.trial = function(display_element, trial) {




///FUNCTION TO GET WEIGHTS FOR MINE OR AGENT
    function getWeightBen (intermediate){
        if (intermediate == [1]){ //pick the latent agent weights - for benev agent it is [1,0]
          final_a = weights_agent;  // L side
          final_b = weights_agent; // C side
          final_c = weights_agent; // R side
        } else { //pick the mine probability weight [x,1-x]
          final_a  = trial.weights[0]; //L side
          final_b = trial.weights[1]; //C side
          final_c = trial.weights[2]; //R side
      }
      return [final_a, final_b, final_c];
      }
  
  
  // FUNCTION TO DETERMINE FEEDBACK (1/0) BASED ON WEIGHTS
        function getRandom (weights) {
          var num = Math.random(),
          s = 0,
          lastIndex = weights.length - 1;
  
          for (var i = 0; i < lastIndex; ++i) {
            s += weights[i];
            if (num < s) {
              return results[i];
            }
          }
  
          return results[lastIndex];
        };
  
  
        if (trial.latent_intervention == 1){  //if the array determines that the latent agent should intervene, then use latent agent weights
        var feedback_prob_post = 1;
        } else {
        feedback_prob_post = 0;
        }
  
        // Determine weight to use
        var final_weight = getWeightBen(feedback_prob_post) //final weight decision
  
  
        // Determine which mine was chosen
        var prev_choice = jsPsych.data.getLastTrialData().select('mine_chosen').values;
  
  
        if (prev_choice == 0) { // if they chose the L mine
          input_weight = final_weight[0]; //use left side mine prob
        } else if (prev_choice == 1) {
          input_weight = final_weight[1]; //use center side mine prob
        } else if (prev_choice == 2) {
          input_weight = final_weight[2]; // use right side mine prob
        }
  
  
  
        var final_feedback = getRandom(input_weight) //get final feedback decision
  
  
        if(final_feedback == 1){
          var amount = trial.bonus_amount + 0.05
        } else {
          var amount = trial.bonus_amount
        }

        

        

        ////// DISPLAY BONUS and PHASE


        if(trial.bonus_phase == ""){
            var html = "<p class='center-content' style='color:black'><strong>BONUS:</strong> $" + amount.toFixed(2) + "</p>"
        } else {
            var html = trial.bonus_phase;
        }






        if (prev_choice == 0){
            var side = "left"
             html += '<img src="'+trial.stimuli[0].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } else if (prev_choice == 1){
            var side = "center"
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[1].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } else if (prev_choice == 2) {
            var side = "right"
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
             html += '<img src="'+trial.stimuli[2].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
        } 



      // display feedback
      if (side == 'left' && final_feedback == 1){ //win and left
        var stimulus_shown = 'win'
         html += '<img src="'+trial.feedback_image[1]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd left_fb"></img>'

      } else if (side == 'center' && final_feedback == 1) { //win and center
        var stimulus_shown = 'win'
         html += '<img src="'+trial.feedback_image[1]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd"></img>'

      } else if (side == 'right' && final_feedback == 1) { //win and right
        var stimulus_shown = 'win'
         html += '<img src="'+trial.feedback_image[1]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd right_fb"></img>'

      } else if (side == 'left' && final_feedback == 0) { //loss and left
        var stimulus_shown = 'loss'
         html += '<img src="'+trial.feedback_image[0]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd left_fb"></img>'

      } else if (side == 'center' && final_feedback == 0) { //loss and center
        var stimulus_shown = 'loss'
         html += '<img src="'+trial.feedback_image[0]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd"></img>'

      } else if (side == 'right' && final_feedback == 0) { //loss and right
        var stimulus_shown = 'loss'
         html += '<img src="'+trial.feedback_image[0]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd right_fb"></img>'
      }

  //show prompt if there is one

  if (final_feedback == 1){
    var outcome = "copper"
  } else if (final_feedback == 0) {
    var outcome = "iron"
  }


//show prompt if there is one
if (trial.prompt !== "") {
    html += trial.prompt;
} else{
    html += "<p class='center-content'>You found " + outcome + ".</p>";
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
        "gamble_feedback": final_feedback,
        "mine_prob_win_left": trial.weights[0],  //probability of win for L mine in this block
        "mine_prob_win_center": trial.weights[1],  //probability of win for center mine in this block
        "mine_prob_win_right": trial.weights[2], //probability of win for R mine in this block
        "latent_intervention": trial.latent_intervention,
        "reveal": trial.reveals,
        "prev_choice": prev_choice
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
