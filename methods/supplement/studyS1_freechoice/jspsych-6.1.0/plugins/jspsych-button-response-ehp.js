/**
 * jspsych-image-button-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["button-response-ehp"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'button-response-ehp',
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
     key_correct:{
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Key correct',
        default: null,
        description: 'Correct answer.'
     },
     stimulus: {
        type: jsPsych.plugins.parameterType.HTML_STRING,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The HTML string to be displayed'
      }
    }
  }

  plugin.trial = function(display_element, trial) {




    var part = 0;
    // var choice = -1;

    setup_display1 = function(){
    // display stimulus
        var html = '<img src="'+trial.stimulus+'" id="jspsych-image-button-response-stimulus" style="';
        if(trial.stimulus_height !== null){
        html += 'height:'+trial.stimulus_height+'px; '
        if(trial.stimulus_width == null && trial.maintain_aspect_ratio){
            html += 'width: auto; ';
        }
        }
        if(trial.stimulus_width !== null){
        html += 'width:'+trial.stimulus_width+'px; '
        if(trial.stimulus_height == null && trial.maintain_aspect_ratio){
            html += 'height: auto; ';
        }
        }
        html +='"></img>';

            //show prompt if there is one
    if (trial.prompt !== null) {
        html += trial.prompt;
      }


        //display buttons
        var buttons = [];
        if (Array.isArray(trial.button_html)) {
          if (trial.button_html.length == trial.choices.length) {
            buttons = trial.button_html;
          } else {
            console.error('Error in html-button-response plugin. The length of the button_html array does not equal the length of the choices array');
          }
        } else {
          for (var i = 0; i < trial.choices.length; i++) {
            buttons.push(trial.button_html);
          }
        }
        html += '<div id="jspsych-html-button-response-btngroup">';
        for (var i = 0; i < trial.choices.length; i++) {
          var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
          html += '<div class="jspsych-html-button-response-button" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'" id="jspsych-html-button-response-button-' + i +'" data-choice="'+i+'">'+str+'</div>';
        }
        html += '</div>';
    
        display_element.innerHTML = html;

      
    // add event listeners to buttons
    for (var i = 0; i < trial.choices.length; i++) {
        display_element.querySelector('#jspsych-html-button-response-button-' + i).addEventListener('click', function(e){
          var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
          after_response(choice);
        });
      }

    }
    
    
    setup_display2 = function(){
        var html = '<div id="jspsych-tree-stim-no-response" style="h1"></div>';
        display_element.innerHTML = html;
    }
    


      display_stimuli = function(stage){
		if (stage == 2){
      // console.log('choice before response is  ' + choice)
      if (keychoice != trial.key_correct){
        display_element.querySelector('#jspsych-tree-stim-no-response').append('Incorrect. Try again!')
      } else{
        display_element.querySelector('#jspsych-tree-stim-no-response').append('Correct!')
      }

	}
    }
    

    // store response
    var response = {
        rt: -1,
        button: -1
        };
      

    // start time
    var start_time = performance.now();


    // store response
    var response = {
      rt: null,
      button: null
    };

    // function to handle responses by the subject
    function after_response(choice) {
        keychoice = choice
        console.log(choice)
        
      // measure rt
      var end_time = performance.now();
      var rt = end_time - start_time;
      response.button = choice;
      response.rt = rt;

      // after a valid response, the stimulus will have the CSS class 'responded'

      if (part == 1){
                display_element.innerHTML = '';
                setup_display2();
                display_stimuli(2);
        console.log('choice after response is  ' + choice)
        var feedback_time = setTimeout(function(){

        if (choice != trial.key_correct){
            console.log('incorrect')
            part = 0
            display_element.innerHTML = '';
            next_part();
        } else {
            console.log('correct')
            end_trial();
        }
        },2000);
        setTimeoutHandlers.push(feedback_time);
        }

      // disable all the buttons after a response
      var btns = document.querySelectorAll('.jspsych-html-button-response-button button');
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
        "button_pressed": response.button
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-html-button-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    next_part = function(){
        part = part + 1
        setup_display1();


        console.log("work this time")

            // if (trial.timing_response > 0) {
            //   var t2 = setTimeout(function() {
            //     end_trial();
            //   }, trial.timing_response);
            //   setTimeoutHandlers.push(t2);
            // }
    };
    next_part();



    // // end trial if time limit is set
    // if (trial.trial_duration !== null) {
    //   jsPsych.pluginAPI.setTimeout(function() {
    //     end_trial();
    //   }, trial.trial_duration);
    // }

  };

  return plugin;
})();