import React from 'react';
import { DeletePersonDocument, PersonDocument, PersonWithFullLinksFragment } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonForm } from '@app/ui/EditPersonForm';
import { formatAgeGroup, formatDefaultEventName, formatEventType, fullDateFormatter, numericDateFormatter } from '@/ui/format';
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
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger, DropdownMenuTriggerDots } from './dropdown';
import { useRouter } from 'next/router';
import { buttonCls } from './style';
import { Plus, UserCheck2, UserX2 } from 'lucide-react';
import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { tenantId } from '@/tenant/config';
import { AddToCohortForm } from './AddToCohortForm';
import { CreateCoupleForm } from './CreateCoupleForm';
import { CreateInvitationForm } from './CreateInvitationForm';
import { QRPayment } from './QRPayment';
import { AddToPersonButton } from './AddToPersonForm';
import { CurrentTenantDocument } from '@/graphql/Tenant';

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
      label: <>Členství</>,
      contents: <Memberships key="memberships" item={item} />,
    }
  ];
  if (item.eventAttendancesList.length > 0) {
    tabs.push({
      id: 'events',
      label: <>Účasti</>,
      contents: (
        <div key="events">
          {item.eventAttendancesList?.filter(x => x.instance).map((item) => (
            <EventButton key={item.id} instance={item.instance!} showTrainer showDate />
          ))}
        </div>
      ),
    });
  }
  if (perms.isAdmin || perms.isCurrentPerson(item.id)) {
    tabs.push({
      id: 'payment',
      label: <>Platby</>,
      contents: <Payments key="payments" item={item} />,
    });
    tabs.push({
      id: 'access',
      label: <>Přístupy {item.userProxiesList.length > 0 ? <UserCheck2 /> : <UserX2 />}</>,
      contents: <Access key="access" item={item} />,
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
        </dl>
      </div>

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </>
  );
}

function Access({ item }: { item: PersonWithFullLinksFragment }) {
  const [inviteOpen, setInviteOpen] = React.useState(false);


  return (
    <div className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Přístupové údaje</h3>
        <AddToPersonButton person={item} />
      </div>

      {item.userProxiesList?.map(item => (
        <EditUserProxyCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Pozvánky</h3>

        <Dialog open={inviteOpen} onOpenChange={setInviteOpen}>
          <DialogTrigger asChild>
            <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
              <Plus />
              Přidat
            </button>
          </DialogTrigger>
          <DialogContent>
            <CreateInvitationForm person={item} onSuccess={() => setInviteOpen(false)} />
          </DialogContent>
        </Dialog>
      </div>

      {item.personInvitationsList?.map(item => (
        <div key={item.id}>
          {item.email}, vytvořena {fullDateFormatter.format(new Date(item.createdAt))}
        </div>
      ))}
    </div>
  );
}

function Payments({ item }: { item: PersonWithFullLinksFragment }) {
  const [{data: tenant}] = useQuery({query: CurrentTenantDocument});

  return (
    <div className="prose prose-accent mb-2">
      {item.unpaidPayments.length > 0 && <h3>K zaplacení</h3>}
      {item.unpaidPayments.map(x => (
        <div key={x.id}>
          {x.payment?.cohortSubscription && (
            <h4>Členské příspěvky {x.payment.cohortSubscription.cohort?.sName}</h4>
          )}
          {x.priceList?.map((price, i) => (
            <div key={i}>
              <dl className="mb-2">
                <dt>Částka</dt>
                <dd>{price?.amount} {price?.currency === 'CZK' ? 'Kč' : price?.currency}</dd>
                <dt>Účet</dt>
                <dd>1806875329/0800</dd>
                <dt>Variabilní symbol</dt>
                <dd>{x.payment?.variableSymbol}</dd>
                <dt>Specifický symbol</dt>
                <dd>{x.payment?.specificSymbol}</dd>
                <dt>Zpráva</dt>
                <dd>{item.firstName + ' ' + item.lastName + ', ' + x.payment?.cohortSubscription?.cohort?.sName}</dd>
                {x.payment?.dueAt && (
                  <>
                    <dt>Splatnost</dt>
                    <dd>{fullDateFormatter.format(new Date(x.payment?.dueAt))}</dd>
                  </>
                )}
              </dl>

              {tenant?.tenant?.bankAccount && (
                <QRPayment
                  key={i}
                  acc={tenant.tenant.bankAccount}
                  am={price?.amount}
                  cc={price?.currency || 'CZK'}
                  ss={x.payment?.specificSymbol}
                  vs={x.payment?.variableSymbol}
                  msg={item.firstName + ' ' + item.lastName + ', ' + x.payment?.cohortSubscription?.cohort?.sName}
                />
              )}
            </div>
          ))}
        </div>
      ))}

      {item.tentativePayments.length > 0 && <h3>Nadcházející</h3>}
      {item.tentativePayments.map(x => (
        <div key={x.id}>
          {x.priceList?.map((x, i) => (
            <div key={i}>{x?.amount} {x?.currency}</div>
          ))}
          {x.payment?.status}
        </div>
      ))}

      {item.accountsList.length === 0 && <p>Žádné evidované platby</p>}
      {item.accountsList?.map(item => (
        <div key={item.id}>
          Stav účtu: {Math.round(parseFloat(item.balance) * 100) / 100} Kč
          <div>
            <h3>Minulé</h3>
            {item.postings.nodes.map(x => {
              let date = x?.transaction?.payment?.paidAt;
              let description = '';

              let event = x.transaction?.payment?.eventInstance?.event
              if (event) {
                description = parseFloat(x.amount) < 0 ? ((formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')) : formatDefaultEventName(event);
                date = x.transaction?.payment?.eventInstance?.since
              }

              event = x.transaction?.payment?.eventRegistration?.event;
              if (event) {
                description = formatDefaultEventName(event);
                date = event.eventInstancesList?.[0]?.since;
              }

              const cohort = x.transaction?.payment?.cohortSubscription?.cohort
              if (cohort) {
                description = `Příspěvky: ${cohort.sName}`;
              }

              return (
                <div key={x.id} className="justify-between gap-2 flex flex-wrap">
                  <span>
                    {date ? numericDateFormatter.format(new Date(date)) : ''}{' '}
                    {description}
                  </span>
                  <span>{Math.round(parseFloat(x.amount) * 100) / 100} Kč</span>
                </div>
              );
            })}
          </div>
        </div>
      ))}
    </div>
  );
}

function Memberships({ item }: { item: PersonWithFullLinksFragment }) {
  const { perms } = useAuth();
  const [coupleOpen, setCoupleOpen] = React.useState(false);
  const [cohortOpen, setCohortOpen] = React.useState(false);
  const createTenantMember = useMutation(CreateTenantMembershipDocument)[1];
  const createTenantTrainer = useMutation(CreateTenantTrainerDocument)[1];
  const createTenantAdmin = useMutation(CreateTenantAdministratorDocument)[1];

  return (
    <div key="info" className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Páry</h3>

        {perms.isAdmin && (
          <Dialog open={coupleOpen} onOpenChange={setCoupleOpen} modal={false}>
            <DialogTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DialogTrigger>
            <DialogContent>
              <CreateCoupleForm initial={item} onSuccess={() => setCoupleOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.allCouplesList?.map((item) => (
        <EditCoupleCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Tréninkové skupiny</h3>

        {perms.isAdmin && (
          <Dialog open={cohortOpen} onOpenChange={setCohortOpen} modal={false}>
            <DialogTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DialogTrigger>
            <DialogContent>
              <AddToCohortForm person={item} onSuccess={() => setCohortOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </div>
      {item.cohortMembershipsList.map((item) => (
        <EditCohortMembershipCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Členství</h3>

        {perms.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuButton
                onClick={() => createTenantAdmin({ input: { tenantAdministrator: { personId: item.id, tenantId } } })}
              >
                jako správce
              </DropdownMenuButton>
              <DropdownMenuButton
                onClick={() => createTenantTrainer({ input: { tenantTrainer: { personId: item.id, tenantId } } })}
              >
                jako trenéra
              </DropdownMenuButton>
              <DropdownMenuButton
                onClick={() => createTenantMember({ input: { tenantMembership: { personId: item.id, tenantId } } })}
              >
                jako člena
              </DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </div>

      {item.tenantAdministratorsList.map((item) => (
        <EditTenantAdministratorCard key={item.id} data={item} />
      ))}
      {item.tenantTrainersList.filter(x => x.active).map((item) => (
        <EditTenantTrainerCard key={item.id} data={item} />
      ))}
      {item.tenantMembershipsList.map((item) => (
        <EditTenantMembershipCard key={item.id} data={item} />
      ))}
    </div>
  );
}
