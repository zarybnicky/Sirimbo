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
