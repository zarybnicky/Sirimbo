import { FederationIdIssuesDocument } from '@/graphql/Person';
import { useAuth } from '@/lib/auth';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { FederationIdBackfillWidget } from './FederationIdBackfillWidget';
import { Spinner } from './Spinner';
import { Tab, TabMenu } from './TabMenu';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from './dialog';
import { FederationLink } from './federation-links';
import { FormError } from './form';
import { formatDateOnly } from './format';
import { EditPersonForm } from './forms/EditPersonForm';

type Federation = 'csts' | 'wdsf';

export function FederationIdWidget({ federation }: { federation: Federation }) {
  const [tab, setTab] = useQueryState(
    'ids',
    parseAsString.withDefault('missing').withOptions({ history: 'push' }),
  );
  return (
    <TabMenu selected={tab} onSelect={setTab}>
      <Tab id="missing" title="Doplnit chybějící">
        <FederationIdBackfillWidget federation={federation} />
      </Tab>
      <Tab id="issues" title="Zkontrolovat vyplněné">
        <FederationIdIssues federation={federation} />
      </Tab>
    </TabMenu>
  );
}

function FederationIdIssues({ federation }: { federation: Federation }) {
  const auth = useAuth();
  const showClubs =
    auth.isSystemAdmin || (auth.isAdmin && auth.adminTenantIds.length > 1);
  const [query] = useQuery({
    query: FederationIdIssuesDocument,
    variables: { federation },
  });
  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;

  const idField = federation === 'csts' ? 'cstsId' : 'wdsfId';
  const idLabel = federation === 'csts' ? 'ČSTS IDT' : 'WDSF MIN';
  const members = new Set(
    [...(query.data.current?.nodes ?? []), ...(query.data.former?.nodes ?? [])].map(
      (p) => p.id,
    ),
  );
  const groups = [
    ...Map.groupBy(
      (query.data.people?.nodes ?? []).filter(
        (p) => p[idField] !== null && (p[idField] > 0 || members.has(p.id)),
      ),
      (p) => p[idField]!,
    ),
  ].filter(
    ([id, people]) =>
      (id <= 0 || people.length > 1) && people.some((p) => members.has(p.id)),
  );

  return (
    <div className="grid gap-3 text-sm">
      <FormError error={query.error} />
      {groups.length === 0 && <p>Žádná sdílená ani neplatná ID.</p>}
      {groups.map(([id, people]) => (
        <section key={id} className="grid gap-2 border-b border-neutral-4 pb-3">
          <h3 className="font-bold">
            {idLabel} {id}{' '}
            {id > 0 ? (
              <FederationLink entity="people" federation={federation} externalId={id}>
                {people[0]!.federationPeople.nodes[0]?.canonicalName ||
                  `${idLabel} ${id}`}
              </FederationLink>
            ) : (
              'Neplatné ID'
            )}
          </h3>
          {people.map((person) => (
            <div
              key={person.id}
              className="flex flex-wrap items-center justify-between gap-2 rounded-md bg-neutral-2 px-2 py-1.5"
            >
              <div className="flex flex-wrap items-baseline gap-x-2">
                <Link
                  href={`/clenove/${person.id}`}
                  className="font-bold text-accent-11 underline"
                >
                  {person.name}
                </Link>
                {person.birthDate && (
                  <span className="text-neutral-11">
                    {formatDateOnly(person.birthDate)}
                  </span>
                )}
                {showClubs &&
                  [
                    ...Map.groupBy(
                      [
                        ...person.tenantMembershipsList,
                        ...person.tenantTrainersList,
                        ...person.tenantAdministratorsList,
                      ].filter(
                        (m) =>
                          auth.isSystemAdmin || auth.adminTenantIds.includes(m.tenantId),
                      ),
                      (membership) => membership.tenantId,
                    ),
                  ].map(([tenantId, memberships]) => (
                    <span key={tenantId} className="text-neutral-11">
                      {memberships[0]!.tenantName ?? tenantId}
                      {memberships.every((m) => m.status !== 'ACTIVE') &&
                        (memberships.some((m) => m.status === 'PENDING')
                          ? ' (budoucí)'
                          : ' (bývalý)')}
                    </span>
                  ))}
              </div>
              <Dialog>
                <DialogTrigger.Edit size="sm" />
                <DialogContent className="sm:max-w-2xl">
                  <DialogTitle>{person.name}</DialogTitle>
                  <EditPersonForm id={person.id} />
                </DialogContent>
              </Dialog>
            </div>
          ))}
        </section>
      ))}
    </div>
  );
}
