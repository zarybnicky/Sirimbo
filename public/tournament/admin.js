import * as React from 'react';
export class TournamentAdmin extends React.Component {
    constructor() {
        super(...arguments);
        this.state = {
            tournament: {
                winnersRoot: 0, losersRoot: 0,
                tournamentPlayers: {},
                dashboardFocus: null,
                nodes: {},
            },
        };
        this.socket = null;
        this.vote = () => {
            this.socket.send(JSON.stringify({
                tag: 'UpdatePlayer',
                contents: [0, { shortName: 'short', longName: 'long' }],
            }));
        };
    }
    componentWillMount() {
        this.socket = new WebSocket('ws://localhost:4000/tournament/admin/ws');
        this.socket.onmessage = event => {
            console.log(JSON.parse(event.data));
        };
    }
    render() {
        return React.createElement('main', { className: 'app yomani' }, React.createElement('header', {}, React.createElement('div', {
            className: 'pie',
            style: { backgroundImage: '' },
        }), 'text'), React.createElement('div', { className: 'numbers' }, React.createElement('button', { className: `ok disabled`, onClick: this.vote }, 'OK')));
    }
}
//# sourceMappingURL=admin.js.map