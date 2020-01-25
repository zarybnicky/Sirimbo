import * as React from 'react';
import { Tournament, TournamentNode, Player } from './common';

interface AdminState {
    tournament: Tournament;
}

export class TournamentAdmin extends React.Component<null, AdminState> {
    state = {
        tournament: {
            winnersRoot: 0, losersRoot: 0,
            tournamentPlayers: {},
            dashboardFocus: null,
            userFocus: null,
            nodes: {},
        },
    };
    socket: WebSocket = null as unknown as WebSocket;
    timeout: number | undefined = undefined;

    componentDidMount() {
        this.socket = this.connect();
    }
    connect = () => {
        clearTimeout(this.timeout);
        this.timeout = undefined;
        this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/admin/ws');
        this.socket.onmessage = e => {
            const msg = JSON.parse(e.data);
            if (msg['tag'] === 'StateMsg') {
                this.setState({ tournament: msg.contents });
            }
        }
        this.socket.onclose = () => {
            this.timeout = this.timeout || setTimeout(this.connect, 500);
        };
        this.socket.onerror = () => {
            this.timeout = this.timeout || setTimeout(this.connect, 500);
        };
        return this.socket;
    }

    open = (k: string) => {
        this.socket.send(JSON.stringify({
            tag: 'OpenVoting', contents: parseInt(k, 10),
        }));
    }
    close = (k: string) => {
        this.socket.send(JSON.stringify({
            tag: 'CloseVoting', contents: parseInt(k, 10),
        }));
    }
    focus = (k: string) => {
        this.socket.send(JSON.stringify({
            tag: 'UserFocusNode', contents: parseInt(k, 10),
        }));
    }
    reset = () => {
        this.socket.send(JSON.stringify({ tag: 'ResetState' }));
    }

    renderBattle = (t: Tournament, i: number, k: string) => {
        const n = t.nodes[k];
        if (!n) return null;
        const open = () => this.open(k);
        const close = () => this.close(k);
        const focus = () => this.focus(k);

        return <div key={k} className="row mb-2">
            <div className="col-1 d-flex align-items-center justify-content-end">#{i}</div>
            <div className="col-7">
                <BattleComponent
                    socket={this.socket} tournament={t} battle={n}
                ></BattleComponent>
            </div>
            <div className="col-4">
                {n.tag === 'DuelWaitingNode'
                    ? <button onClick={open}
                        className="btn btn-sm btn-primary">Spustit hlasování</button> : null}
                {n.tag === 'DuelFinishedNode' && n.contents[1].victor === null
                    ? <button onClick={close}
                        className="btn btn-sm btn-primary">Ukončit hlasování</button> : null}
                {(t.userFocus || 0).toString() === k.toString()
                    ? 'Aktuální'
                    : <button onClick={focus}
                        className="btn btn-sm btn-primary">Nastavit jako aktuální</button>}
            </div>
        </div>;
    }

    renderPlayer = (t: Tournament, k: string) => {
        const p = t.tournamentPlayers[k];
        return <PlayerComponent key={k} socket={this.socket} playerId={k} player={p}></PlayerComponent>;
    }

    render() {
        const t = this.state.tournament;
        return <main className='container'>
            <button className="btn btn-danger" onClick={this.reset}>Restartovat turnaj</button>
            <h3>Duely</h3>
            {[2, 5, 8, 1].map((k, i) => this.renderBattle(t, i, k.toString()))}
            <h3>Páry</h3>
            {Object.keys(t.tournamentPlayers).map(k => this.renderPlayer(t, k))}
        </main>;
    }
}

export class BattleComponent extends React.Component<{
    socket: WebSocket; tournament: Tournament; battle: TournamentNode;
}> {
    constructor(props: any) {
        super(props);
    }

    render() {
        const n = this.props.battle;
        const t = this.props.tournament;
        if (n.tag === 'DuelWaitingNode') {
            const l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
            const r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
            return <ul className="list-group">
                <li className="list-group-item d-flex justify-content-between align-items-center">
                    {l ? l.longName : '?'}
                </li>
                <li className="list-group-item d-flex justify-content-between align-items-center">
                    {r ? r.longName : '?'}
                </li>
            </ul>;
        } else if (n.tag === 'DuelFinishedNode') {
            const res = n.contents[1];
            const l = t.tournamentPlayers[res.leftPlayer] || {};
            const r = t.tournamentPlayers[res.rightPlayer] || {};
            return <ul className="list-group">
                <li className="list-group-item d-flex justify-content-between align-items-center">
                    <span className={res.victor === res.leftPlayer ? 'font-weight-bold' : undefined}>
                        {l.longName}
                    </span>
                    <span className="badge badge-primary badge-pill">{res.leftScore}</span>
                </li>
                <li className="list-group-item d-flex justify-content-between align-items-center">
                    <span className={res.victor === res.rightPlayer ? 'font-weight-bold' : undefined}>
                        {r.longName}
                    </span>
                    <span className="badge badge-primary badge-pill">{res.rightScore}</span>
                </li>
            </ul>;
        }
    }
}

export class PlayerComponent extends React.Component<{
    socket: WebSocket; playerId: string; player: Player;
}, { player: Player; }> {
    constructor(props: any) {
        super(props);
        this.state = { player: props.player };
    }
    componentWillReceiveProps(nextProps: any) {
        if (nextProps.player !== this.props.player) {
            this.setState({ player: nextProps.player });
        }
    }
    longChange = (event: any) => {
        const player = { ...this.state.player, longName: event.target.value };
        this.setState({ player });
        this.props.socket.send(JSON.stringify({
            tag: 'UpdatePlayer',
            contents: [parseInt(this.props.playerId), player],
        }))
    }

    shortChange = (event: any) => {
        const player = { ...this.state.player, shortName: event.target.value };
        this.setState({ player });
        this.props.socket.send(JSON.stringify({
            tag: 'UpdatePlayer',
            contents: [parseInt(this.props.playerId), player],
        }))
    }

    render() {
        return <div className="form-row">
            <div className="col">
                <input
                    type="text" className="form-control" placeholder="Dlouhé jméno"
                    value={this.state.player.longName}
                    onChange={this.longChange}
                />
            </div>
            <div className="col">
                <input
                    type="text" className="form-control" placeholder="Krátké jméno"
                    value={this.state.player.shortName}
                    onChange={this.shortChange}
                />
            </div>
        </div>;
    }
}
