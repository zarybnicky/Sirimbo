import * as React from 'react';
export class TournamentClient extends React.Component {
    constructor() {
        super(...arguments);
        this.state = {
            tournament: {
                winnersRoot: 0, losersRoot: 0,
                tournamentPlayers: {},
                dashboardFocus: null,
                nodes: {},
            },
            pastVotes: {},
        };
        this.socket = null;
        this.vote = () => {
        };
    }
    componentDidMount() {
        this.socket = new WebSocket('ws://localhost:4000/tournament/ws');
        this.socket.onmessage = e => {
            console.log(JSON.parse(e.data));
            this.setState({ tournament: JSON.parse(e.data) });
        };
    }
    renderBattle(t, n) {
        if (!n)
            return;
        if (n.tag === 'DuelWaitingNode') {
            return (<div>
                Battle {n.contents[0]}
                {n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]].shortName : '???'}
                versus
              {n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]].shortName : '???'}
            </div>);
        }
        else if (n.tag === 'DuelFinishedNode') {
            return React.createElement('div', {}, 'Battle', n.contents[0], t.tournamentPlayers[n.contents[1].leftPlayer].shortName, ': ', t.tournamentPlayers[n.contents[1].leftScore], React.createElement('br', {}), t.tournamentPlayers[n.contents[1].rightPlayer].shortName, ': ', t.tournamentPlayers[n.contents[1].rightScore], React.createElement('br', {}), 'Vítěz: ', n.contents[1].victor ?
                t.tournamentPlayers[n.contents[1].victor].shortName : '???');
        }
    }
    render() {
        const t = this.state.tournament;
        return React.createElement('main', { className: 'app' }, React.createElement('header', {}, React.createElement('div', {
            className: 'pie',
            style: { backgroundImage: '' },
        }), 'text'), React.createElement('div', { className: 'numbers' }, React.createElement('button', { className: `ok disabled`, onClick: this.vote }, 'OK')), this.renderBattle(t, t.nodes[2]), this.renderBattle(t, t.nodes[5]), this.renderBattle(t, t.nodes[8]), this.renderBattle(t, t.nodes[1]));
    }
}
//# sourceMappingURL=index.js.map