// The claim shape the frontend needs to derive auth state. These are the fields
// baked into the signed JWT by app_private.create_jwt_token; the server returns
// them as plain data (from the auth mutations and the who-am-I endpoint) so the
// frontend never decodes a JWT itself.
export type SessionClaims = {
  user_id?: string;
  tenant_id?: string;
  username?: string;
  email?: string;
  my_person_ids?: string[];
  my_tenant_ids?: string[];
  my_cohort_ids?: string[];
  my_couple_ids?: string[];
  guest_tenant_ids?: string[];
  member_tenant_ids?: string[];
  trainer_tenant_ids?: string[];
  admin_tenant_ids?: string[];
  is_member?: boolean;
  is_trainer?: boolean;
  is_admin?: boolean;
  is_system_admin?: boolean;
};

// Server-side only: extract the claims from a signed token. This is the single
// place a JWT is read, done once to produce a structured return value — it is
// not a verification (the backend verifies on every request); it only turns the
// token the backend just issued into data for the client.
export function decodeClaims(jwt: string | null | undefined): SessionClaims | null {
  const payload = jwt?.split('.')[1];
  if (!payload) return null;
  try {
    return JSON.parse(Buffer.from(payload, 'base64url').toString('utf8')) as SessionClaims;
  } catch {
    return null;
  }
}
