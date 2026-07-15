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

// Read claims out of a signed token to return to the client as data. Not a
// verification — the backend verifies on every request.
export function decodeClaims(jwt: string | null | undefined): SessionClaims | null {
  const payload = jwt?.split('.')[1];
  if (!payload) return null;
  try {
    return JSON.parse(Buffer.from(payload, 'base64url').toString()) as SessionClaims;
  } catch {
    return null;
  }
}
