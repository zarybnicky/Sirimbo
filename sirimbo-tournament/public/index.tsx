import * as React from 'react';
import { Tournament, TournamentNode, Player, DuelResult } from './common';

interface ClientState {
    tournament: Tournament;
    pastVotes: { [key: string]: number; };
}

export class TournamentClient extends React.Component<null, ClientState> {
    state = {
        tournament: {
            winnersRoot: 0,
            losersRoot: 0,
            tournamentPlayers: {},
            dashboardFocus: null,
            nodes: {},
        } as Tournament,
        pastVotes: {},
    };
    socket: WebSocket = null as unknown as WebSocket;
    timeout: number | undefined;

    componentDidMount() {
        this.socket = this.connect();
    }
    connect = () => {
        this.timeout && clearTimeout(this.timeout);
        this.timeout = undefined;
        this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/ws');
        // this.socket = new WebSocket('ws://localhost:4000/tournament/ws');
        this.socket.onmessage = e => {
            const msg = JSON.parse(e.data);
            if (msg['tag'] === 'StateMsg') {
                this.setState({ tournament: msg.contents });
            } else {
                localStorage.clear();
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

    render() {
        const t = this.state.tournament;
        const names: { [key: string]: string } = {
            2: 'Semifinále #1',
            5: 'Semifinále #2',
            8: 'Bitva o třetí místo',
            1: 'Finále',
        };
        return <div>
            <nav className="navbar navbar-dark fixed-top pl-0"
                style={{ backgroundColor: '#e80b30' }}>
                <div className="container pl-0">
                    <a className="navbar-brand m-0" style={{ lineHeight: '45px' }}>
                        <img src="https://tkolymp.cz/style/new-logo-oneline.png"
                            style={{ height: '45px' }} className="d-inline-block align-top" />
                        Bitva o Olomouc
                    </a>
                </div>
            </nav>
            <main className='container' style={{ marginTop: '80px' }}>
                {[2, 5, 8, 1].map(k => <div key={k} className="mb-2">
                    <div className="text-muted">{names[k]}</div>
                    <UserBattleComponent
                        socket={this.socket} tournament={t} battle={t.nodes[k]}
                    ></UserBattleComponent>
                    {(t.nodes[k] || {}).tag !== 'DuelFinishedNode' ? null :
                        <VoteComponent
                            socket={this.socket} tournament={t} battle={t.nodes[k]}
                        ></VoteComponent>}
                </div>)}
            </main>
        </div>;
    }
}

export class VoteComponent extends React.Component<{
    socket: WebSocket; tournament: Tournament; battle: TournamentNode;
}> {
    constructor(props: any) {
        super(props);
    }

    voteLeft = () => {
        if (!this.props.battle) return null;
        const id = (this.props.battle.contents as any)[0];
        const pastVote = localStorage.getItem(id) || null;
        if (pastVote === 'left') {
            this.props.socket.send(JSON.stringify([id, -1, 0]));
            localStorage.removeItem(id);
        } else if (pastVote === 'right') {
            this.props.socket.send(JSON.stringify([id, 1, -1]));
            localStorage.setItem(id, 'left');
        } else if (pastVote === null) {
            this.props.socket.send(JSON.stringify([id, 1, 0]));
            localStorage.setItem(id, 'left');
        }
    }

    voteRight = () => {
        if (!this.props.battle) return null;
        const id = (this.props.battle.contents as any)[0];
        const pastVote = localStorage.getItem(id) || null;
        if (pastVote === 'right') {
            this.props.socket.send(JSON.stringify([id, 0, -1]));
            localStorage.removeItem(id);
        } else if (pastVote === 'left') {
            this.props.socket.send(JSON.stringify([id, -1, 1]));
            localStorage.setItem(id, 'right');
        } else if (pastVote === null) {
            this.props.socket.send(JSON.stringify([id, 0, 1]));
            localStorage.setItem(id, 'right');
        }
    }

    render() {
        if (!this.props.battle) return null;
        const n = this.props.battle;
        const t = this.props.tournament;
        const pastVote = localStorage.getItem((n.contents as any)[0]) || null; //left|right|null
        const enabled = n.tag === 'DuelFinishedNode' && n.contents[1].victor === null;
        if (!enabled && !pastVote) {
            return null;
        }
        let l = null, r = null;
        if (n.tag === 'DuelWaitingNode') {
            l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null
            r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
        } else if (n.tag === 'DuelFinishedNode') {
            const res = n.contents[1];
            l = t.tournamentPlayers[res.leftPlayer] || {};
            r = t.tournamentPlayers[res.rightPlayer] || {};
        }
        return <div>
            <div className="row m-2 mb-5">
                <div className="col-10 col-md-8 mt-2 mb-2">
                    <button
                        disabled={!enabled}
                        onClick={this.voteLeft}
                        className={'btn btn-block text-left ' +
                            (pastVote === 'left' ? 'btn-primary' : 'btn-outline-primary')}
                    >
                        {l ? l.shortName : '?'}
                        {pastVote === 'left' ? ' ✓' : ''}
                    </button>
                </div>
                <div className="col-10 col-md-8 offset-2 offset-md-4">
                    <button
                        disabled={!enabled}
                        onClick={this.voteRight}
                        className={'btn btn-block text-right ' +
                            (pastVote === 'right' ? 'btn-primary' : 'btn-outline-primary')}
                    >
                        {pastVote === 'right' ? '✓ ' : ''}
                        {r ? r.shortName : '?'}
                    </button>
                </div>
            </div>
        </div>;
    }
}

export class UserBattleComponent extends React.Component<{
    socket: WebSocket; tournament: Tournament; battle: TournamentNode;
}> {
    constructor(props: any) {
        super(props);
    }

    render() {
        const n = this.props.battle;
        const t = this.props.tournament;
        if (!n) return null;
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
