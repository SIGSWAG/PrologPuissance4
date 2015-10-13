/*
* ajaxGET --- send a GET ajax call and expect json response
* path : the RELATIVE path to send the request
* params : json object conteining GET parameters
*
* return the json response 
*/
function ajaxGETjson(path, params){
	console.log("AJAX : "+path+" | "+$.param(params));
	return $.ajax({
		method: "GET",
		url: "http://localhost:8000/"+path,
		data: $.param(params),
		error: function (resultat, statut, erreur) {
			console.log("error");
			alert("Erreur lors de l'appel à "+path);
			console.log(resultat, statut, erreur);
		}
	});
}

/* addMsg --- add a message in the msg-box
* msg : the message to be displayed
*/
function addMsg(msg){
	var $msgbox = $("#msg-box");
	$msgbox.append($("<p></p>").addClass("msg").text(msg));
}

var Game = {

	init : function(){
		addMsg("Initialisation du jeu ...");
		ajax = ajaxGETjson('init', {});
		ajax.success(function(json, statut){
			if(json.correct){
				addMsg("Jeu initialisé.")
			}
			else{
				this.error("Erreur lors de l'initialisation du jeu", 1)
			}
		})
	},
	error : function(msg, code){
		addMsg("Désolé, une erreur est survenue.");
		console.log("ERROR CODE : "+code);
		console.log("ERROR MESSAGE : "+msg);
	}

}


// document.ready
$(function(){
	addMsg("Jquery est chargé.");
	Game.init()
});

