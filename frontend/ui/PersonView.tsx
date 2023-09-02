import React from 'react';
import { PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonDialog } from '@app/ui/EditPersonDialog';
import { getAgeGroup } from '@app/ui/get-age-group';
import { EditCohortMembershipCard } from '@app/ui/EditCohortMembershipForm';
import { EditTenantAdministratorCard } from '@app/ui/EditTenantAdministratorForm'
import { EditTenantTrainerCard } from '@app/ui/EditTenantTrainerForm'
import { EditTenantMembershipCard } from '@app/ui/EditTenantMembershipForm'
import { EditCoupleCard } from '@app/ui/EditCoupleForm'
import { typographyCls } from './style';
import { EventButton } from './EventButton';

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

        {!!item.allCouplesList?.length && <h3>Páry</h3>}
        {item.allCouplesList?.map((item) => (
          <EditCoupleCard key={item.id} data={item} />
        ))}

        <h3>Členství</h3>
        {item.tenantAdministratorsList?.map((item) => (
          <EditTenantAdministratorCard key={item.id} data={item} />
        ))}
        {item.tenantTrainersList?.map((item) => (
          <EditTenantTrainerCard key={item.id} data={item} />
        ))}
        {item.tenantMembershipsList?.map((item) => (
          <EditTenantMembershipCard key={item.id} data={item} />
        ))}

        {!!item.cohortMembershipsList?.length && <h3>Tréninkové skupiny</h3>}
        {item.cohortMembershipsList?.map((item) => (
          <EditCohortMembershipCard key={item.id} data={item} />
        ))}

        {perms.isAdmin && !!item.userProxiesList.length && (
          <>
            <h3>Přístupové údaje</h3>
            {item.userProxiesList?.map(item => (
              <div>{item.user?.uLogin}, {item.user?.uEmail}</div>
            ))}
          </>
        )}

        {!!item.eventAttendancesList.length && (
          <>
            <h2>Poslední události</h2>
            {item.eventAttendancesList?.map((item) => (
              <EventButton key={item.id} instance={item.instance!} showTrainer showDate />
            ))}
          </>
        )}
      </div>
    </>
  );
}
