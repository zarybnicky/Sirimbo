import type { DevelopmentUsersQuery } from '@/graphql/CurrentUser';
import { normalizeClaims, resolveAuth } from '@/lib/auth-claims';

export function getDevelopmentUsers(
  users: NonNullable<DevelopmentUsersQuery['users']>['nodes'],
  tenantId: string,
  now: number,
) {
  const recentCutoff = now - 90 * 86_400_000;
  return users.flatMap((user) => {
    const persons = user.userProxiesList.flatMap((x) => x.person ?? []);
    const claims = normalizeClaims(user.claims);
    if (!claims || persons.length === 0) return [];
    return {
      id: user.id,
      email: user.uEmail,
      name: persons.map((person) => person.name).join(', '),
      role: resolveAuth(claims, tenantId).role,
      recentlyActive: persons.some(
        ({ lastActivity }) => lastActivity?.trainingAt &&
          Date.parse(lastActivity.trainingAt) >= recentCutoff,
      ),
    };
  });
}

export function pickDevelopmentUser(
  users: ReturnType<typeof getDevelopmentUsers>,
  currentUserId: string | undefined,
) {
  const others = users.filter((user) => user.id !== currentUserId);
  const available = others.length > 0 ? others : users;
  const recent = available.filter((user) => user.recentlyActive);
  const choices = recent.length > 0 ? recent : available;
  return choices[Math.floor(Math.random() * choices.length)];
}
