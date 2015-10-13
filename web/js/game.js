var port = 8000;





/*
 var params = {
     idee_id: idee,
     com: msg
 };

 var paramsEncoded = $.param(params);
 $.ajax({
     method: "POST",
     url: "{% url 'jacquesIdea:ajax_enregistrer_commentaire' %}",
     data: paramsEncoded,
     dataType: 'html',
     cache: false,
     success: function (html, statut) {
         var scrollToId = $(html).find('.direct-chat-txt').attr("id");
         $(html).appendTo('#messages-container-' + idee);
         var objDiv = document.getElementById('messages-container-' + idee);
         objDiv.scrollTop = objDiv.scrollHeight;
         $("#message-text-" + idee).val("");
     },
     error: function (resultat, statut, erreur) {
         alert("Désolé ! Une erreur serveur est survenue, veuillez réessayer.");
     }
 });
*/

/*
* ajaxGET --- send a GET ajax call and expect json response
* path : the RELATIVE path to send the request
* params : json object conteining GET parameters
*
* return the json response 
*/
function ajaxGETjson(path, params){
	$.ajax({
		method: "GET",
		url: "http://localhost:"+port+"/"+path,
		data: $.param(params),
		crossDomain: true,
		//dataType: 'json',
		success: function (json, statut) {
			return json;
		},
		error: function (resultat, statut, erreur) {
			alert("Erreur lors de l'appel à "+path);
			console.log(resultat, statut, erreur);
			return {};
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

// document.ready
$(function(){
	addMsg("Jquery est chargé.");
	var response;
	response = ajaxGETjson('init', {});
	addMsg(response);
});