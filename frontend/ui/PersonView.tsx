import React from 'react';
import { DeletePersonDocument, PersonDocument, PersonPageFragment } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonForm } from '@app/ui/EditPersonForm';
import { formatAgeGroup, formatDefaultEventName, formatEventType, fullDateFormatter, moneyFormatter, numericDateFormatter } from '@/ui/format';
import { EventButton } from './EventButton';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { useConfirm } from './Confirm';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { useRouter } from 'next/router';
import { UserCheck2, UserX2 } from 'lucide-react';
import { QRPayment } from './QRPayment';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { TransactionExportButton } from './TransactionExportButton';
import { CreateCreditTransactionButton } from './CreateCreditTransactionForm';
import { PersonAccessView } from './PersonAccessView';
import { PersonMembershipView } from './PersonMembershipView';
import { attendanceIcons } from './AttendanceView';

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
      contents: <PersonMembershipView key="memberships" item={item} />,
    }
  ];
  if (item.eventAttendancesList.length > 0) {
    tabs.push({
      id: 'events',
      label: <>Účasti</>,
      contents: (
        <div key="events" className="grid grid-cols-[1fr_50px]">
          {item.eventAttendancesList?.filter(x => x.instance).map((item) => (
            <React.Fragment key={item.id}>
              <EventButton instance={item.instance!} showTrainer showDate />
              <div className="flex items-center justify-center">
                {React.createElement(attendanceIcons[item.status], { className: "w-5 h-5" })}
              </div>
            </React.Fragment>
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
      contents: <PersonAccessView key="access" item={item} />,
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

      <dl className="mb-2">
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

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </>
  );
}

function Payments({ item }: { item: PersonPageFragment }) {
  const [{ data: tenant }] = useQuery({query: CurrentTenantDocument});
  const person = item;

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
              <dl className="not-prose mb-2">
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
          <div className="flex flex-wrap justify-between">
            <div>Stav kreditu: {moneyFormatter.format(parseFloat(item.balance))}</div>
            <div className="flex gap-2">
              <TransactionExportButton name={person.name || ''} postings={item.postings.nodes || []} />
              <CreateCreditTransactionButton account={item} />
            </div>
          </div>

          <div>
            <h3>Minulé</h3>
            {item.postings.nodes.map((x) => {
              let date = x?.transaction?.payment?.paidAt || x!.transaction!.createdAt;
              let description = x.transaction?.description;

              let event = x.transaction?.payment?.eventInstance?.event
              if (event) {
                description = parseFloat(x.amount) < 0 ? ((formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')) : formatDefaultEventName(event);
                date = x.transaction?.payment?.eventInstance?.since || date
              }

              event = x.transaction?.payment?.eventRegistration?.event;
              if (event) {
                description = formatDefaultEventName(event);
                date = event.eventInstancesList?.[0]?.since || date
              }

              const cohort = x.transaction?.payment?.cohortSubscription?.cohort
              if (cohort) {
                description = `Příspěvky: ${cohort.sName}`;
              }

              return { id: x.id, date, description, amount: x.amount };
            }).sort((a, b) => a.date < b.date ? 1 : a.date > b.date ? -1 : 0).map(x => (
              <div key={x.id} className="justify-between gap-2 flex flex-wrap">
                <span>
                  {numericDateFormatter.format(new Date(x.date))}{' '}
                  {x.description}
                </span>
                <span>{moneyFormatter.format(parseFloat(x.amount))}</span>
              </div>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}

