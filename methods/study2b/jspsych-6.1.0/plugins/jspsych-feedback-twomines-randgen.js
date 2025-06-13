/**
 * jspsych-image-button-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["feedback-twomines-randgen"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'feedback-twomines-randgen',
    description: '',
    parameters: {
          min: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Min slider',
            default: 0,
            description: 'Sets the minimum value of the slider.'
          },
          max: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Max slider',
            default: 100,
            description: 'Sets the maximum value of the slider',
          },
          start: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Slider starting value',
            default: 50,
            description: 'Sets the starting value of the slider',
          },
          step: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Step',
            default: 1,
            description: 'Sets the step of the slider'
          },
          labels: {
            type: jsPsych.plugins.parameterType.HTML_STRING,
            pretty_name:'Labels',
            default: [],
            array: true,
            description: 'Labels of the slider.',
          },
          slider_width: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name:'Slider width',
            default: 400,
            description: 'Width of the slider in pixels.'
          },
          button_label: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Button label',
            default:  'Continue',
            array: false,
            description: 'Label of the button to advance.'
          },
          require_movement: {
            type: jsPsych.plugins.parameterType.BOOL,
            pretty_name: 'Require movement',
            default: true, //CHANGE THE DEFAULT TO true TO REQUIRE SLIDER MOVEMENT
            description: 'If true, the participant will have to move the slider before continuing.'
          },
          prompt: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Prompt',
            default: null,
            description: 'Any content here will be displayed below the slider.'
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
          response_ends_trial: {
            type: jsPsych.plugins.parameterType.BOOL,
            pretty_name: 'Response ends trial',
            default: true,
            description: 'If true, trial will end when user makes a response.'
          },
          block_id:{
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Condition',
            default: null,
            description: 'Which condition is the participant in.'
          }, 
          weights:{
            type: jsPsych.plugins.parameterType.FLOAT,
            pretty_name: 'Mine weights',
            default: [999,999],
            array: true,
            description: 'True value of each mine.'
          }, 
          stimuli:{
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Image stimuli',
            default: null,
            array: true,
            description: 'Source for images to display.'
          },
          feedback_image:{
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Feedback image stimuli',
            default: null,
            array: true,
            description: 'Source for feedback images to display.'
          },
          bonus_phase:{
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Bonus amount and phase',
            default: "",
            description: 'Text to display for bonus and phase.'
          },
          latent_intervention:{
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Latent Intervention',
            default: 999,
            description: 'Whether or not an intervention occurs on this trial.'
          },
          reveals:{
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Reveal cause',
            default: null,
            description: 'Whether or not the cause is revealed on this trial.'
          },
          feedback_forced:{
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Feedback',
            default: null,
            description: 'Pre-defined feedback on this trial.'          
          }
        }
      }


  plugin.trial = function(display_element, trial) {





    ///FUNCTION TO GET WEIGHTS FOR MINE OR AGENT
    function getWeightBen (intermediate){
        if (intermediate == [1]){ //pick the latent agent weights - for benev agent it is [1,0]
            final_a = weights_agent;  // L side
            final_b = weights_agent; // R side
        } else { //pick the mine probability weight [x,1-x]
            final_a  = trial.weights[0]; //L side
            final_b = trial.weights[1]; //R side
        }
            return [final_a, final_b];
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



        // Determine which mine was chosen
        var prev_choice = jsPsych.data.getLastTrialData().select('mine_chosen').values;



        console.log(trial.feedback_forced)



        ///Get feedback, pre-defined or determined by generative model
        if(trial.feedback_forced !== null){
            var final_feedback = trial.feedback_forced;
        } else {
            if (trial.latent_intervention == 1){  //if the array determines that the latent agent should intervene, then use latent agent weights
              var feedback_prob_post = 1;
            } else {
                feedback_prob_post = 0;
            }

            // Determine weight to use
            var final_weight = getWeightBen(feedback_prob_post) //final weight decision

            if (prev_choice == 0) { // if they chose the L mine
              input_weight = final_weight[0]; //use left side mine prob
              } else if (prev_choice == 1) {
              input_weight = final_weight[1]; //use right side mine prob
              }

            var final_feedback = getRandom(input_weight) //get final feedback decision
        }


        ////// DISPLAY BONUS and PHASE

        var html = trial.bonus_phase;



        if (prev_choice == 0){
            var side = "left"
            html += '<div class="fb-img" ><img src="'+trial.stimuli[0].unsel+'" class="jspsych-xab-stimulus left" style="width: 33.3%" ></img>'
            html += '<img src="'+outcome_blank+'" class="jspsych-xab-stimulus right" style="width: 33.3%"></img>'
        } else if (prev_choice == 1){
            var side = "right"
            html += '<div class="fb-img" ><img src="'+outcome_blank+'" class="jspsych-xab-stimulus left" style="width: 33.3%"></img>'
            html += '<img src="'+trial.stimuli[1].unsel+'" class="jspsych-xab-stimulus right" style="width: 33.3%"></img>'
        } 


    // display feedback
    if (side == 'left' && final_feedback == 1){ //win and left
        var stimulus_shown = 'win'
        html += '<img src="'+trial.feedback_image[1]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd left_fb"></img></div>'

    } else if (side == 'right' && final_feedback == 1) { //win and right
        var stimulus_shown = 'win'
        html += '<img src="'+trial.feedback_image[1]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd right_fb"></img></div>'

    } else if (side == 'left' && final_feedback == 0) { //loss and left
        var stimulus_shown = 'loss'
        html += '<img src="'+trial.feedback_image[0]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd left_fb"></img></div>'

    } else if (side == 'right' && final_feedback == 0) { //loss and right
        var stimulus_shown = 'loss'
        html += '<img src="'+trial.feedback_image[0]+'" id="jspsych-sim-stim" class="jspsych-feedback-hmd right_fb"></img></div>'

    }

            //show prompt if there is one
        if (trial.prompt !== null) {
            html += trial.prompt;
            html += "<br>(You must click or move the slider to continue)"
        }
        




        html += '<div id="jspsych-html-slider-response-wrapper" style="margin: 30px 0px;">';
        html += '<div class="jspsych-html-slider-response-container" style="position:relative; margin: 0 auto 3em auto; ';
        if(trial.slider_width !== null){
          html += 'width:'+trial.slider_width+'px;';
        }
        html += '">';
        html += '<input type="range" value="'+trial.start+'" min="'+trial.min+'" max="'+trial.max+'" step="'+trial.step+'" style="width: 100%;" id="jspsych-html-slider-response-response"></input>';
        html += '<div>'
        for(var j=0; j < trial.labels.length; j++){
          var width = 100/(trial.labels.length-1);
          var left_offset = (j * (100 /(trial.labels.length - 1))) - (width/2);
          html += '<div style="display: inline-block; position: absolute; left:'+left_offset+'%; text-align: center; width: '+width+'%;">';
          html += '<span style="text-align: center; font-size: 80%;">'+trial.labels[j]+'</span>';
          html += '</div>'
        }
        html += '</div>';
        html += '</div>';
        html += '</div>';

        
    // add submit button
    html += '<button id="jspsych-html-slider-response-next" class="jspsych-btn" '+ (trial.require_movement ? "disabled" : "") + '>'+trial.button_label+'</button>';

    display_element.innerHTML = html;

    var response = {
      rt: null,
      response: null
    };
    
    if(trial.require_movement){
      display_element.querySelector('#jspsych-html-slider-response-response').addEventListener('click', function(){
        display_element.querySelector('#jspsych-html-slider-response-next').disabled = false;
      })
    }

    display_element.querySelector('#jspsych-html-slider-response-next').addEventListener('click', function() {
      // measure response time
      var endTime = performance.now();
      response.rt = endTime - startTime;
      response.response = display_element.querySelector('#jspsych-html-slider-response-response').value;

      if(trial.response_ends_trial){
        end_trial();
      } else {
        display_element.querySelector('#jspsych-html-slider-response-next').disabled = true;
      }

    });

    function end_trial(){

      jsPsych.pluginAPI.clearAllTimeouts();

      // save data
      var trialdata = {
        "rt": response.rt,
        "rating": response.response,
        "prev_choice": prev_choice,
        "gamble_feedback": final_feedback,
        "mine_prob_win_left": trial.weights[0],  //probability of win for L mine in this block
        "mine_prob_win_right": trial.weights[1],  //probability of win for center mine in this block
        "latent_intervention": trial.latent_intervention,
        "reveal": trial.reveals
      };

      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trialdata);
    }

    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-html-slider-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if trial_duration is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

    var startTime = performance.now();
  };

  return plugin;
})();
