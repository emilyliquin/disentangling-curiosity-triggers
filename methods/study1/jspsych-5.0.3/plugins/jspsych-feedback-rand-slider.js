

jsPsych.plugins["feedback-rand-slider"] = (function() {

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    // default trial parameters


    trial.min = trial.min || 0;
    trial.max = trial.max || 100;
    trial.start = trial.start || 50;
    trial.step = trial.step || 1;
    trial.labels = trial.labels;
    trial.button_label = trial.button_label || 'Continue';
    trial.response_ends_trial = (typeof trial.response_ends_trial === 'undefined') ? true : trial.response_ends_trial;
    trial.timing_stim = trial.timing_stim || -1; // if -1, then show indefinitely
    trial.timing_response = trial.timing_response || -1; // if -1, then wait for response forever
    trial.is_html = (typeof trial.is_html === 'undefined') ? false : trial.is_html;
    trial.prompt = (typeof trial.prompt === 'undefined') ? "" : trial.prompt;
    trial.block_id = trial.block_id;
    trial.feedback_image = trial.feedback_image; //added
    trial.agent_image = trial.agent_image; //added
    trial.choice_seq = trial.choice_seq || null; //added
    trial.feedback_seq = trial.feedback_seq; //added
    trial.stimuli = trial.stimuli;
    trial.require_movement = trial.require_movement || true;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    // this array holds handlers from setTimeout calls
    // that need to be cleared if the trial ends early
    var setTimeoutHandlers = [];



                if (trial.choice_seq == 1){
                  var side = "left"
                  if (!trial.is_html) {
                    display_element.append($('<img>', {
                      "src": trial.stimuli[0].sel,
                      //"id": 'jspsych-sim-stim',
                      class: 'jspsych-xab-stimulus left'
                    }));
                    display_element.append($('<img>', {
                      "src": outcome_blank,
                      //"id": 'jspsych-sim-stim',
                      class: 'jspsych-xab-stimulus right'
                    }));
                  } else {
                    display_element.append($('<div>', {
                      "html": trial.stimuli[0].sel,
                      //"id": 'jspsych-sim-stim',
                      class: 'jspsych-xab-stimulus left'
                    }));
                    display_element.append($('<div>', {
                      "html": outcome_blank,
                      //"id": 'jspsych-sim-stim',
                      class: 'jspsych-xab-stimulus right'
                    }));
                  }
                  } else {
                    side = "right"
                    if (!trial.is_html) {
                      display_element.append($('<img>', {
                        "src": outcome_blank,
                        //"id": 'jspsych-sim-stim',
                        class: 'jspsych-xab-stimulus left'
                      }));
                      display_element.append($('<img>', {
                        "src": trial.stimuli[1].sel,
                        //"id": 'jspsych-sim-stim',
                        class: 'jspsych-xab-stimulus right'
                      }));
                    } else {
                      display_element.append($('<div>', {
                        "html": outcome_blank,
                        //"id": 'jspsych-sim-stim',
                        class: 'jspsych-xab-stimulus left'
                      }));
                      display_element.append($('<div>', {
                        "html": trial.stimuli[1].sel,
                        //"id": 'jspsych-sim-stim',
                        class: 'jspsych-xab-stimulus right'
                      }));
                    }
                  }


                  var final_feedback = trial.feedback_seq


                  // display feedback
                  if (side == 'left' && final_feedback == 1){ //win and left
                    var stimulus_shown = 'win'
                    display_element.append($('<img>', {
                      "src": trial.feedback_image[1],
                      "id": 'jspsych-sim-stim',
                      // class: 'feedback-design'
                      class: 'inner left_fb'
                      // class: 'jspsych-xab-stimulus left'
                    }));
                  } else if (side == 'right' && final_feedback == 1) { //win and right
                    var stimulus_shown = 'win'
                    display_element.append($('<img>', {
                      "src": trial.feedback_image[1],
                      "id": 'jspsych-sim-stim',
                      // class: 'feedback-design'
                      class: 'inner right_fb'
                      // class: 'jspsych-xab-stimulus right'
                    }));
                  } else if (side == 'left' && final_feedback == 0) { //loss and left
                    var stimulus_shown = 'loss'
                    display_element.append($('<img>', {
                      "src": trial.feedback_image[0],
                      "id": 'jspsych-sim-stim',
                      // class: 'feedback-design'
                      class: 'inner left_fb'
                      // class: 'jspsych-xab-stimulus left'
                    }));
                  } else if (side == 'right' && final_feedback == 0) { //loss and right
                    var stimulus_shown = 'loss'
                    display_element.append($('<img>', {
                      "src": trial.feedback_image[0],
                      "id": 'jspsych-sim-stim',
                      // class: 'feedback-design'
                      class: 'inner right_fb'
                      // class: 'jspsych-xab-stimulus right'
                    }));
                  }

              //show prompt if there is one

              if (trial.prompt !== "") {
                display_element.append(trial.prompt 
                  + "<p font-size: small> (You must click or move the slider to continue) </p> "
                  );
              }



              //display slider
              
              var html  ='<div class="jspsych-html-slider-response-container" style="position:relative;">';
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

              display_element.append(html)



              // store response
              var response = {
                rt: null,
                response: null
              };


              // add submit button
              display_element.append($('<button>', {
                'id': 'jspsych-survey-text-next',
                'class': 'jspsych-btn jspsych-survey-text',
                'style': 'top:100px',
                'disabled': true /// HERE - SET TO TRUE IF YOU WANT TO REQUIRE CLICK
              }));


              $("#jspsych-html-slider-response-response").on('click', function(){
                document.getElementById("jspsych-survey-text-next").disabled = false;
              })

              $("#jspsych-survey-text-next").html(trial.button_label);


              // mousedown doesn't seem to be working
              $("#jspsych-survey-text-next").mousedown(function() {
                if(document.getElementById("jspsych-survey-text-next").disabled == true){
                  display_element.append("<p style='color:red'> You must move the slider to continue </p>")
                }
                
              });


              $("#jspsych-survey-text-next").click(function() {
                // measure response time
                var endTime = (new Date()).getTime();
                var response_time = endTime - startTime;
                response.response = document.getElementById('jspsych-html-slider-response-response').value;
                console.log(response.response);
                      // save data
                var trialdata = {
                  "rt": response_time,
                  "response": response.response,
                  "block_id": trial.block_id,
                  "gamble_feedback": final_feedback //feedback for the gamble (0 for loss, 1 for win)

                };

                display_element.html('');

                // next trial
                jsPsych.finishTrial(trialdata);

                });

                  
        
            var startTime = (new Date()).getTime();
            };



            return plugin;
          })();
