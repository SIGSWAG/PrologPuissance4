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
		},
		complete: function(response){
			console.log(response.responseJSON);
		}
	});
}

/* addMsg --- add a message in the msg-box
* msg : the message to be displayed
*/
function addMsg(msg, type){
	type = typeof type !== 'undefined' ? type : 'info';
	var $msgbox = $("#msg-box");
	$msgbox.prepend($("<p></p>").addClass("msg bg-"+type).text(msg));
}

function Player(){
	this.code = 0;
	this.Name = '';
	this.color = '';
}

var Game = {
	player1 : null,
	player2 : null,
	currentPlayer : null,

	init : function(){
		addMsg("Initialisation du jeu ...",'primary');
		Game.player1 = new Player();
		Game.player2 = new Player();
		currentPlayer = null;
		var ajax = ajaxGETjson('init', {});
		ajax.success(function(json, statut){
			if(json.correct){
				addMsg("Jeu initialisé.","success");
				Game.playerSelection(json.players);
			}
			else{
				Game.error("Erreur lors de l'initialisation du jeu.", 1)
			}
		})
	},
	playerSelection : function(playersList){
		var $playerSelection =  $("#player-selection").removeClass("hide");
		for (var i = 0; i < playersList.length; i++) {
			var playerId = playersList[i][0];
			var playerName = playersList[i][1];
			var $selectableElement = $("<p></p>")
				.attr("data-id", playerId)
				.attr("data-name", playerName)
				.addClass("selectable player")
				.text(playerName);
			$selectableElement.click(function(){
				if(Game.player1.code === 0){
					Game.player1.code = $(this).attr("data-id");
					Game.player1.name = $(this).attr("data-name");
					addMsg(Game.player1.name+" a été séléctionné.","success");
					addMsg("Sélectionnez le joueur 2.");
				}
				else if(Game.player2.code === 0){
					Game.player2.code = $(this).attr("data-id");
					Game.player2.name = $(this).attr("data-name");
					addMsg(Game.player2.name+" a été séléctionné.","success");
					$playerSelection.addClass("hide");
					addMsg("Confirmation de la selection des joueurs ...","primary");
					var ajax = ajaxGETjson('selectPlayers', {joueur1:Game.player1.code, joueur2:Game.player2.code});
					ajax.success(function(json, statut){
						if(json.correct){
							addMsg("Selection des joueurs confirmée.","success");
							if(json.rouge == Game.player1.code){
								Game.player1.color = 'rouge';
								Game.player2.color = 'jaune';
								Game.currentPlayer = Game.player1;
							}
							else{
								Game.player1.color = 'jaune';
								Game.player2.color = 'rouge';
								Game.currentPlayer = Game.player2;
							}
							addMsg("Le joueur "+Game.player1.name+" est "+Game.player1.color,"success");
							addMsg("Le joueur "+Game.player2.name+" est "+Game.player2.color,"success");
							Game.playTurn();
						}
						else{
							Game.error("Erreur lors de la confirmation des joueurs.", 3);
						}
					})
				}
				else{
					Game.error("Game.playerSelection a été appelé, mais les deux joueurs sont déjà définis.", 2)
				}

			});
			$playerSelection.append($selectableElement);
		};
		addMsg("Sélectionnez le joueur 1.");
	},
	playTurn : function(){
		addMsg("C'est le tour de "+Game.currentPlayer.name+"("+Game.currentPlayer.color+")");
		// is it human or IA ?
		if(Game.currentPlayer.code == 1){
			// Human

		}
		else{
			// IA
			var ajax = ajaxGETjson('playFromIA', {});
			ajax.success(function(json, statut){
				if(json.correct){
					addMsg(Game.currentPlayer.name+"("+Game.currentPlayer.color+") joue en [col,row] : ["+json.colPlayed+","+json.rowPlayed+"]", Game.currentPlayer.color);
					Game.insertToken(json.colPlayed, json.rowPlayed);
					switch(json.gameStatus){
						case "continue" :
							Game.switchPlayer();
							Game.playTurn();
							break;
						case "win" :
							addMsg(Game.currentPlayer.name+"("+Game.currentPlayer.color+") a gagné la partie !", "success");
							break;
						default :
							Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+". gameStatus : '"+json.gameStatus+"' est inconnu.", 5);
					}
				}
				else{
					Game.error("Erreur lors de la récupération du coup de "+Game.currentPlayer.name+".", 4);
				}
			});
		}
	},
	insertToken : function(col,row){
		var $cell = $("#cell-"+col+"-"+row);
		$cell.html('<div class="board-token '+Game.currentPlayer.color+'"></div>');
	},
	switchPlayer : function(){
		/*
		if (this.currentPlayer === this.player1){
			this.currentPlayer = this.player2;
		} else{
			this.currentPlayer = this.player1;
		};
		*/
		this.currentPlayer = (this.currentPlayer === this.player1)?this.player2:this.player1;
	},
	error : function(msg, code){
		addMsg("Désolé, une erreur est survenue.","danger");
		console.log("ERROR CODE : "+code);
		console.log("ERROR MESSAGE : "+msg);
	}

};


// document.ready
$(function(){
	addMsg("Jquery est chargé.","success");
	Game.init()
});

