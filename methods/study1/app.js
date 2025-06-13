// --- LOADING MODULES
var express = require('express');
    mongoose = require('mongoose'),
    body_parser = require('body-parser');


require('dotenv').config();

// --- INSTANTIATE THE APP
var app = express();


// var emptySchema = new mongoose.Schema({}, { strict: false });
// var Entry = mongoose.model('Entry', emptySchema);

// let uri = process.env.MONGODB_URI;

// mongoose.connect(uri);
// let db = mongoose.connection;

// db.on('error', console.error.bind(console, 'connection error:'));

// db.once('open', function callback() {
//     console.log('database opened');
// });


// --- STATIC MIDDLEWARE 
app.use(express.static(__dirname + '/public'));
app.use("/jspsych-5.0.3", express.static(__dirname + "/jspsych-5.0.3"));
app.use("/css", express.static(__dirname + "/css"));
app.use("/img", express.static(__dirname + "/img"));
app.use("/sequences", express.static(__dirname + "/sequences"));

app.use(body_parser.json());


// --- VIEW LOCATION, SET UP SERVING STATIC HTML
app.set('views', __dirname + '/public/views');
app.engine('html', require('ejs').renderFile);
app.set('view engine', 'html');

// --- ROUTING

app.get('/', function(request, response) {
    response.render('index.html');
});


// MODIFY HERE TO CHANGE THE STUDY
app.get('/experiment', function(request, response) {     
    response.render('Study1a.html');
});


app.get('/finish', function(request, response) {
    response.render('finish.html');
});

app.post('/experiment-data', function(request, response){
    Entry.create({
        "data":request.body
    });    
    response.end();
})

// --- START THE SERVER 
var server = app.listen(process.env.PORT, function(){
    console.log("Listening on port %d", server.address().port);
});