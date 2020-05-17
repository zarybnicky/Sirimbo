define("common", ["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
});
define("admin", ["require", "exports", "react"], function (require, exports, React) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TournamentAdmin extends React.Component {
        constructor() {
            super(...arguments);
            this.state = {
                tournament: {
                    winnersRoot: 0, losersRoot: 0,
                    tournamentPlayers: {},
                    dashboardFocus: null,
                    userFocus: null,
                    nodes: {},
                },
            };
            this.socket = null;
            this.timeout = undefined;
            this.connect = () => {
                this.timeout && clearTimeout(this.timeout);
                this.timeout = undefined;
                this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/admin/ws');
                this.socket.onmessage = e => {
                    const msg = JSON.parse(e.data);
                    if (msg['tag'] === 'StateMsg') {
                        this.setState({ tournament: msg.contents });
                    }
                };
                this.socket.onclose = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                this.socket.onerror = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                return this.socket;
            };
            this.open = (k) => {
                this.socket.send(JSON.stringify({
                    tag: 'OpenVoting', contents: parseInt(k, 10),
                }));
            };
            this.close = (k) => {
                this.socket.send(JSON.stringify({
                    tag: 'CloseVoting', contents: parseInt(k, 10),
                }));
            };
            this.reset = () => {
                this.socket.send(JSON.stringify({ tag: 'ResetState' }));
            };
            this.names = {
                2: 'Semifinále #1',
                5: 'Semifinále #2',
                8: 'Bitva o třetí místo',
                1: 'Finále',
            };
            this.renderBattle = (t, k) => {
                const n = t.nodes[k];
                if (!n)
                    return null;
                const open = () => this.open(k);
                const close = () => this.close(k);
                return React.createElement("div", { key: k, className: "row mb-2" },
                    React.createElement("div", { className: "col-12 col-md-2 d-block d-md-flex align-items-center justify-content-end" }, this.names[k]),
                    React.createElement("div", { className: "col-12 col-md-7" },
                        React.createElement(BattleComponent, { socket: this.socket, tournament: t, battle: n })),
                    React.createElement("div", { className: "col-12 col-md-3 text-right" },
                        n.tag === 'DuelWaitingNode'
                            ? React.createElement("button", { onClick: open, className: "btn btn-sm btn-primary" }, "Spustit hlasov\u00E1n\u00ED") : null,
                        n.tag === 'DuelFinishedNode' && n.contents[1].victor === null
                            ? React.createElement("button", { onClick: close, className: "btn btn-sm btn-primary" }, "Ukon\u010Dit hlasov\u00E1n\u00ED") : null));
            };
            this.renderPlayer = (t, k) => {
                const p = t.tournamentPlayers[k];
                return React.createElement(PlayerComponent, { key: k, socket: this.socket, playerId: k, player: p });
            };
        }
        componentDidMount() {
            this.socket = this.connect();
        }
        render() {
            const t = this.state.tournament;
            return React.createElement("main", { className: 'container' },
                React.createElement("button", { className: "btn btn-danger", onClick: this.reset }, "Restartovat turnaj"),
                React.createElement("h3", null, "Duely"),
                [2, 5, 8, 1].map(k => this.renderBattle(t, k.toString())),
                React.createElement("h3", null, "P\u00E1ry"),
                Object.keys(t.tournamentPlayers).map(k => this.renderPlayer(t, k)));
        }
    }
    exports.TournamentAdmin = TournamentAdmin;
    class BattleComponent extends React.Component {
        constructor(props) {
            super(props);
        }
        render() {
            const n = this.props.battle;
            const t = this.props.tournament;
            if (n.tag === 'DuelWaitingNode') {
                const l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                const r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, l ? l.longName : '?'),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, r ? r.longName : '?'));
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                const l = t.tournamentPlayers[res.leftPlayer] || {};
                const r = t.tournamentPlayers[res.rightPlayer] || {};
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.leftPlayer ? 'font-weight-bold' : undefined }, l.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.leftScore)),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.rightPlayer ? 'font-weight-bold' : undefined }, r.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.rightScore)));
            }
        }
    }
    exports.BattleComponent = BattleComponent;
    class PlayerComponent extends React.Component {
        constructor(props) {
            super(props);
            this.longChange = (event) => {
                const player = Object.assign({}, this.state.player, { longName: event.target.value });
                this.setState({ player });
                this.props.socket.send(JSON.stringify({
                    tag: 'UpdatePlayer',
                    contents: [parseInt(this.props.playerId), player],
                }));
            };
            this.shortChange = (event) => {
                const player = Object.assign({}, this.state.player, { shortName: event.target.value });
                this.setState({ player });
                this.props.socket.send(JSON.stringify({
                    tag: 'UpdatePlayer',
                    contents: [parseInt(this.props.playerId), player],
                }));
            };
            this.state = { player: props.player };
        }
        componentWillReceiveProps(nextProps) {
            if (nextProps.player !== this.props.player) {
                this.setState({ player: nextProps.player });
            }
        }
        render() {
            return React.createElement("div", { className: "form-row" },
                React.createElement("div", { className: "col" },
                    React.createElement("input", { type: "text", className: "form-control", placeholder: "Dlouh\u00E9 jm\u00E9no", value: this.state.player.longName, onChange: this.longChange })),
                React.createElement("div", { className: "col" },
                    React.createElement("input", { type: "text", className: "form-control", placeholder: "Kr\u00E1tk\u00E9 jm\u00E9no", value: this.state.player.shortName, onChange: this.shortChange })));
        }
    }
    exports.PlayerComponent = PlayerComponent;
});
define("index", ["require", "exports", "react"], function (require, exports, React) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TournamentClient extends React.Component {
        constructor() {
            super(...arguments);
            this.state = {
                tournament: {
                    winnersRoot: 0,
                    losersRoot: 0,
                    tournamentPlayers: {},
                    dashboardFocus: null,
                    nodes: {},
                },
                pastVotes: {},
            };
            this.socket = null;
            this.connect = () => {
                this.timeout && clearTimeout(this.timeout);
                this.timeout = undefined;
                this.socket = new WebSocket('wss://api.tkolymp.cz/tournament/ws');
                this.socket.onmessage = e => {
                    const msg = JSON.parse(e.data);
                    if (msg['tag'] === 'StateMsg') {
                        this.setState({ tournament: msg.contents });
                    }
                    else {
                        localStorage.clear();
                    }
                };
                this.socket.onclose = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                this.socket.onerror = () => {
                    this.timeout = this.timeout || setTimeout(this.connect, 500);
                };
                return this.socket;
            };
        }
        componentDidMount() {
            this.socket = this.connect();
        }
        render() {
            const t = this.state.tournament;
            const names = {
                2: 'Semifinále #1',
                5: 'Semifinále #2',
                8: 'Bitva o třetí místo',
                1: 'Finále',
            };
            return React.createElement("div", null,
                React.createElement("nav", { className: "navbar navbar-dark fixed-top pl-0", style: { backgroundColor: '#e80b30' } },
                    React.createElement("div", { className: "container pl-0" },
                        React.createElement("a", { className: "navbar-brand m-0", style: { lineHeight: '45px' } },
                            React.createElement("img", { src: "https://tkolymp.cz/style/new-logo-oneline.png", style: { height: '45px' }, className: "d-inline-block align-top" }),
                            "Bitva o Olomouc"))),
                React.createElement("main", { className: 'container', style: { marginTop: '80px' } }, [2, 5, 8, 1].map(k => React.createElement("div", { className: "mb-2" },
                    React.createElement("div", { className: "text-muted" }, names[k]),
                    React.createElement(UserBattleComponent, { socket: this.socket, tournament: t, battle: t.nodes[k] }),
                    (t.nodes[k] || {}).tag !== 'DuelFinishedNode' ? null :
                        React.createElement(VoteComponent, { socket: this.socket, tournament: t, battle: t.nodes[k] })))));
        }
    }
    exports.TournamentClient = TournamentClient;
    class VoteComponent extends React.Component {
        constructor(props) {
            super(props);
            this.voteLeft = () => {
                if (!this.props.battle)
                    return null;
                const id = this.props.battle.contents[0];
                const pastVote = localStorage.getItem(id) || null;
                if (pastVote === 'left') {
                    this.props.socket.send(JSON.stringify([id, -1, 0]));
                    localStorage.removeItem(id);
                }
                else if (pastVote === 'right') {
                    this.props.socket.send(JSON.stringify([id, 1, -1]));
                    localStorage.setItem(id, 'left');
                }
                else if (pastVote === null) {
                    this.props.socket.send(JSON.stringify([id, 1, 0]));
                    localStorage.setItem(id, 'left');
                }
            };
            this.voteRight = () => {
                if (!this.props.battle)
                    return null;
                const id = this.props.battle.contents[0];
                const pastVote = localStorage.getItem(id) || null;
                if (pastVote === 'right') {
                    this.props.socket.send(JSON.stringify([id, 0, -1]));
                    localStorage.removeItem(id);
                }
                else if (pastVote === 'left') {
                    this.props.socket.send(JSON.stringify([id, -1, 1]));
                    localStorage.setItem(id, 'right');
                }
                else if (pastVote === null) {
                    this.props.socket.send(JSON.stringify([id, 0, 1]));
                    localStorage.setItem(id, 'right');
                }
            };
        }
        render() {
            if (!this.props.battle)
                return null;
            const n = this.props.battle;
            const t = this.props.tournament;
            const pastVote = localStorage.getItem(n.contents[0]) || null;
            const enabled = n.tag === 'DuelFinishedNode' && n.contents[1].victor === null;
            if (!enabled && !pastVote) {
                return null;
            }
            let l = null, r = null;
            if (n.tag === 'DuelWaitingNode') {
                l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                l = t.tournamentPlayers[res.leftPlayer] || {};
                r = t.tournamentPlayers[res.rightPlayer] || {};
            }
            return React.createElement("div", null,
                React.createElement("div", { className: "row m-2 mb-5" },
                    React.createElement("div", { className: "col-10 col-md-8 mt-2 mb-2" },
                        React.createElement("button", { disabled: !enabled, onClick: this.voteLeft, className: 'btn btn-block text-left ' +
                                (pastVote === 'left' ? 'btn-primary' : 'btn-outline-primary') },
                            l ? l.shortName : '?',
                            pastVote === 'left' ? ' ✓' : '')),
                    React.createElement("div", { className: "col-10 col-md-8 offset-2 offset-md-4" },
                        React.createElement("button", { disabled: !enabled, onClick: this.voteRight, className: 'btn btn-block text-right ' +
                                (pastVote === 'right' ? 'btn-primary' : 'btn-outline-primary') },
                            pastVote === 'right' ? '✓ ' : '',
                            r ? r.shortName : '?'))));
        }
    }
    exports.VoteComponent = VoteComponent;
    class UserBattleComponent extends React.Component {
        constructor(props) {
            super(props);
        }
        render() {
            const n = this.props.battle;
            const t = this.props.tournament;
            if (!n)
                return null;
            if (n.tag === 'DuelWaitingNode') {
                const l = n.contents[1] !== null ? t.tournamentPlayers[n.contents[1]] : null;
                const r = n.contents[2] !== null ? t.tournamentPlayers[n.contents[2]] : null;
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, l ? l.longName : '?'),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" }, r ? r.longName : '?'));
            }
            else if (n.tag === 'DuelFinishedNode') {
                const res = n.contents[1];
                const l = t.tournamentPlayers[res.leftPlayer] || {};
                const r = t.tournamentPlayers[res.rightPlayer] || {};
                return React.createElement("ul", { className: "list-group" },
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.leftPlayer ? 'font-weight-bold' : undefined }, l.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.leftScore)),
                    React.createElement("li", { className: "list-group-item d-flex justify-content-between align-items-center" },
                        React.createElement("span", { className: res.victor === res.rightPlayer ? 'font-weight-bold' : undefined }, r.longName),
                        React.createElement("span", { className: "badge badge-primary badge-pill" }, res.rightScore)));
            }
        }
    }
    exports.UserBattleComponent = UserBattleComponent;
});
//# sourceMappingURL=bundle.js.map