import React from 'react';
import { DeletePersonDocument, PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonForm } from '@app/ui/EditPersonForm';
import { formatAgeGroup } from '@/ui/format';
import { EditCohortMembershipCard } from '@app/ui/EditCohortMembershipForm';
import { EditTenantAdministratorCard } from '@app/ui/EditTenantAdministratorForm'
import { EditTenantTrainerCard } from '@app/ui/EditTenantTrainerForm'
import { EditTenantMembershipCard } from '@app/ui/EditTenantMembershipForm'
import { EditCoupleCard } from '@app/ui/EditCoupleForm'
import { EventButton } from './EventButton';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { EditUserProxyCard } from './EditUserProxyForm';
import { useConfirm } from './Confirm';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { useRouter } from 'next/router';

export function PersonView({ id }: { id: string }) {
  const { perms } = useAuth();
  const router = useRouter();
  const [{ data }] = useQuery({ query: PersonDocument, variables: { id }, pause: !id });
  const [variant, setVariant] = useQueryParam('tab', StringParam);
  const confirm = useConfirm();
  const deleteMutation = useMutation(DeletePersonDocument)[1];
  const [editOpen, setEditOpen] = React.useState(false);

  const item = data?.person;
  if (!item) {
    return null;
  }

  const tabs = [
    {
      id: 'info',
      label: 'Členství',
      contents: (
        <div key="info" className="prose prose-accent mb-2">
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
                <EditUserProxyCard key={item.id} data={item} />
              ))}
            </>
          )}
        </div>
      ),
    }
  ];
  if (item.eventAttendancesList.length > 0) {
    tabs.push({
      id: 'events',
      label: 'Účasti',
      contents: (
        <div key="events">
          {item.eventAttendancesList?.filter(x => x.instance).map((item) => (
            <EventButton key={item.id} instance={item.instance!} showTrainer showDate />
          ))}
        </div>
      ),
    });
  }

  return (
    <>
      <TitleBar title={item.name}>
        {(perms.isAdmin || perms.isCurrentPerson(item.id)) && (
          <DropdownMenu>
            <DropdownMenuTriggerDots />
            <DropdownMenuContent align="end">
              <Dialog open={editOpen} onOpenChange={setEditOpen}>
                <DialogTrigger asChild>
                  <DropdownMenuButton onSelect={(e) => e.preventDefault()}>
                    Upravit
                  </DropdownMenuButton>
                </DialogTrigger>
                <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
                  <EditPersonForm data={item} onSuccess={() => setEditOpen(false)} />
                </DialogContent>
              </Dialog>

              {perms.isAdmin && !perms.isCurrentPerson(item.id)  && (
                <DropdownMenuButton
                  onClick={async () => {
                    await confirm({ description: `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!` });
                    await deleteMutation({ id })
                    router.replace('/clenove')
                  }}
                >
                  Smazat
                </DropdownMenuButton>
              )}
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </TitleBar>

      <div className="prose prose-accent mb-2">
        <dl>
          {item.birthDate && (
            <>
              <dt>Věková kategorie</dt>
              <dd>{formatAgeGroup(item)}</dd>
            </>
          )}
          {item.phone && (
            <>
              <dt>Telefon</dt>
              <dd>{item.phone}</dd>
            </>
          )}
          {item.email && (
            <>
              <dt>E-mail</dt>
              <dd>{item.email}</dd>
            </>
          )}
          {/* <dt> Variabilní symbol</dt>
          <dd>{(item.legacyUserId || item.nationalIdNumber || item.id).padStart(6, '0')}</dd> */}
        </dl>
      </div>

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </>
  );
}
