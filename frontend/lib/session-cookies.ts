// Shared between server (sets both) and client (reads the presence marker).
export const SESSION_COOKIE = 'rozpisovnik';
// Permanent, not a migration shim: public pages render statically (no cookies()
// on the server), so the header's login state hydrates from this marker client-side.
export const PRESENCE_COOKIE = 'rozpisovnik_p';
