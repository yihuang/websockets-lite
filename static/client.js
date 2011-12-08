function createSockjs(path) {
    var host = window.location.host;
    if(host == '') host = 'localhost';
    var uri = 'http://' + host + path;
    return new Sockjs(uri);
}

var users = [];

function refreshUsers() {
    $('#users').html('');
        console.log(users);
    for(i in users) {
        $('#users').append($(document.createElement('li')).text(users[i]));
    }
}

function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);

    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});

    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ .*/, '');
        users.push(user);
        refreshUsers();
    }

    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }
}

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();
        var ws = createSockjs('/');

        ws.onopen = function() {
            ws.send('join ' + user);
        };

        ws.onmessage = function(event) {
            if(event.data.match('^Welcome! Users: ')) {
                /* Calculate the list of initial users */
                var str = event.data.replace(/^Welcome! Users: /, '');
                if(str != "") {
                    users = str.split(", ");
                    refreshUsers();
                }

                $('#join-section').hide();
                $('#chat-section').show();
                $('#users-section').show();

                ws.onmessage = onMessage;

                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                    return false;
                });
            } else {
                $('#warnings').append(event.data);
                ws.close();
            }
        };

        $('#join').append('Connecting...');

        return false;
    });
});