import { PersonPaymentsDocument } from "@/graphql/Person";
import React from "react";
import { formatDefaultEventName, formatEventType, fullDateFormatter, moneyFormatter, numericDateFormatter } from "./format";
import { useQuery } from "urql";
import { QRPayment } from "./QRPayment";
import { TransactionExportButton } from "./TransactionExportButton";
import { CreateCreditTransactionButton } from "./CreateCreditTransactionForm";
import { useTenant } from "./useTenant";

export function PersonPaymentsView({ id }: { id: string }) {
  const { data: tenant } = useTenant();
  const [query] = useQuery({ query: PersonPaymentsDocument, variables: { id }, pause: !id });
  const item = query.data?.person;
  const person = item;

  if (!item) {
    return null;
  }

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

              {tenant?.bankAccount && (
                <QRPayment
                  key={i}
                  acc={tenant.bankAccount}
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

      {item.accountsList.length === 0 && <p>Žádné evidované platby</p>}
      {item.accountsList?.map(item => (
        <div key={item.id}>
          <div className="flex flex-wrap justify-between">
            <div>Stav kreditu: {moneyFormatter.format(parseFloat(item.balance))}</div>
            <div className="flex gap-2">
              <TransactionExportButton name={person?.name || ''} postings={item.postings.nodes || []} />
              <CreateCreditTransactionButton account={item} />
            </div>
          </div>

          <div>
            <h3>Minulé</h3>
            {item.postings.nodes.map((x) => {
              let date = x?.transaction?.effectiveDate!;
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
                date = x.transaction?.payment?.dueAt || date;
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
