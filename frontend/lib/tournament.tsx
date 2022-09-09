import * as React from 'react';

interface AdminState {
  tournament: Tournament;
}
interface ClientState {
  tournament: Tournament;
  pastVotes: { [key: string]: number; };
}
export interface Player {
  shortName: string;
  longName: string;
}

interface Dictionary<T> {
  [key: string]: T;
}
export interface Tournament {
  winnersRoot: number;
  losersRoot: number;
  tournamentPlayers: Dictionary<Player>;
  dashboardFocus: number | null;
  userFocus: number | null;
  nodes: Dictionary<TournamentNode>;
}

export type TournamentNode
  = { tag: 'SeedNode'; contents: [number, number]; }
  | { tag: 'SeedWaitingNode'; contents: number; }
  | { tag: 'DuelWaitingNode'; contents: [number, number | null, number | null, number, number]; }
  | { tag: 'DuelFinishedNode'; contents: [number, DuelResult, number, number]; }

export interface DuelResult {
  leftPlayer: number;
  rightPlayer: number;
  leftScore: number;
  rightScore: number;
  victor: number | null;
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
  timeout: NodeJS.Timeout | undefined;

  componentDidMount() {
    this.socket = this.connect();
  }
  connect = () => {
    this.timeout && clearTimeout(this.timeout);
    this.timeout = undefined;
    this.socket = new WebSocket('wss://api.tkolymp.cz/api/tournament/ws');
    // this.socket = new WebSocket('ws://localhost:4000/api/tournament/ws');
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
            socket={this.socket} tournament={t} battle={t.nodes[k]!}
          ></UserBattleComponent>
          {(t.nodes[k] || {}).tag !== 'DuelFinishedNode' ? null :
            <VoteComponent
              socket={this.socket} tournament={t} battle={t.nodes[k]!}
            ></VoteComponent>}
        </div>)}
      </main>
    </div>;
  }
}

export class VoteComponent extends React.Component<{
  socket: WebSocket; tournament: Tournament; battle: TournamentNode;
}> {
  voteLeft() {
    if (!this.props.battle) return;
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

  voteRight() {
    if (!this.props.battle) return;
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
      const l = t.tournamentPlayers[res.leftPlayer] || { longName: '' };
      const r = t.tournamentPlayers[res.rightPlayer] || { longName: '' };
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
    } else {
      return null;
    }
  }
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
  timeout: NodeJS.Timeout | undefined = undefined;

  componentDidMount() {
    this.socket = this.connect();
  }
  connect = () => {
    this.timeout && clearTimeout(this.timeout);
    this.timeout = undefined;
    this.socket = new WebSocket('wss://api.tkolymp.cz/api/tournament/admin/ws');
    // this.socket = new WebSocket('ws://localhost:4000/api/tournament/admin/ws');
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
  reset = () => {
    this.socket.send(JSON.stringify({ tag: 'ResetState' }));
  }
  names: { [key: string]: string } = {
    2: 'Semifinále #1',
    5: 'Semifinále #2',
    8: 'Bitva o třetí místo',
    1: 'Finále',
  };

  renderBattle = (t: Tournament, k: string) => {
    const n = t.nodes[k];
    if (!n) return null;
    const open = () => this.open(k);
    const close = () => this.close(k);

    return <div key={k} className="row mb-2">
      <div className="col-12 col-md-2 d-block d-md-flex align-items-center justify-content-end">{this.names[k]}</div>
      <div className="col-12 col-md-7">
        <BattleComponent
          socket={this.socket} tournament={t} battle={n}
        ></BattleComponent>
      </div>
      <div className="col-12 col-md-3 text-right">
        {n.tag === 'DuelWaitingNode'
          ? <button onClick={open}
            className="btn btn-sm btn-primary">Spustit hlasování</button> : null}
        {n.tag === 'DuelFinishedNode' && n.contents[1].victor === null
          ? <button onClick={close}
            className="btn btn-sm btn-primary">Ukončit hlasování</button> : null}
      </div>
    </div>;
  }

  renderPlayer = (t: Tournament, k: string) => {
    const p = t.tournamentPlayers[k]!;
    return <PlayerComponent key={k} socket={this.socket} playerId={k} player={p}></PlayerComponent>;
  }

  render() {
    const t = this.state.tournament;
    return <main className='container'>
      <button className="btn btn-danger" onClick={this.reset}>Restartovat turnaj</button>
      <h3>Duely</h3>
      {[2, 5, 8, 1].map(k => this.renderBattle(t, k.toString()))}
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
      const l = t.tournamentPlayers[res.leftPlayer] || { longName: '' };
      const r = t.tournamentPlayers[res.rightPlayer] || { longName: '' };
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
    } else {
      return null;
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
  UNSAFE_componentWillReceiveProps(nextProps: any) {
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
