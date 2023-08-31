import React from 'react';
import { PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';
import { formatLongCoupleName } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonDialog } from '@app/ui/EditPersonDialog';
import { formatOpenDateRange } from '@app/ui/format';
import Link from 'next/link';
import { getAgeGroup } from '@app/ui/get-age-group';

export function PersonView({ id }: { id: string }) {
  const { perms } = useAuth();
  const [{ data }] = useQuery({ query: PersonDocument, variables: { id }, pause: !id });

  const item = data?.person;

  if (!item) {
    return null;
  }

  return (
    <>
      <TitleBar title={item.name}>
        {(perms.isAdmin || perms.isCurrentPerson(item.id)) && (
          <EditPersonDialog id={id} />
        )}
      </TitleBar>

      <div className="prose prose-accent mb-2">
        <dl>
          <dt>Věková kategorie</dt>
          <dd>{item.birthDate ? getAgeGroup(new Date(item.birthDate).getFullYear()) : "?"}</dd>
          <dt> Variabilní symbol</dt>
          <dd>{(item.legacyUserId || item.nationalIdNumber || item.id).padStart(6, '0')}</dd>
          <dt>Telefon</dt>
          <dd>{item.phone}</dd>
          <dt>E-mail</dt>
          <dd>{item.email}</dd>
        </dl>

        <h3>Členství</h3>
        {item.tenantMembershipsList?.map((item) => (
          <div key={item.id}>Člen klubu {item.tenant?.name} {formatOpenDateRange(item)}</div>
        ))}
        {item.tenantAdministratorsList?.map((item) => (
          <div key={item.id}>Správce klubu {formatOpenDateRange(item)}</div>
        ))}
        {item.tenantTrainersList?.map((item) => (
          <div key={item.id}>Trenér {formatOpenDateRange(item)}</div>
        ))}

        {!!item.couplesList?.length && <h3>Páry</h3>}
        {item.couplesList?.map((item) => (
          <div key={item.id}>
            <Link href={`/pary/${item.id}`}>{formatLongCoupleName(item)}</Link> (
            {formatOpenDateRange(item)})
          </div>
        ))}

        {!!item.cohortMembershipsList?.length && <h3>Tréninkové skupiny</h3>}
        {item.cohortMembershipsList?.map((item) => (
          <div key={item.id}>
            <Link href={`/treninkove-skupiny/${item.cohort?.id}`}>
              {item.cohort?.sName}
            </Link>{' '}
            ({formatOpenDateRange(item)})
          </div>
        ))}
      </div>
    </>
  );
}
