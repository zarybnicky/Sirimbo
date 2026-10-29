import { DefinitionList } from '@/ui/DefinitionList';
import { PersonPaymentsDocument, PersonPaymentsQuery } from '@/graphql/Person';
import { useActionMap } from '@/lib/actions';
import { compareText } from '@/lib/compare';
import { cn } from '@/lib/cn';
import { paymentActions } from '@/lib/actions/payment';
import { transactionActions } from '@/lib/actions/transaction';
import { isTruthy, keyIsNonNull } from '@/lib/truthyFilter';
import { ActionGroup } from '@/ui/ActionGroup';
import {
  describePosting,
  fullDateFormatter,
  moneyFormatter,
  numericDateWithYearFormatter,
} from '@/ui/format';
import { QRPayment } from '@/ui/QRPayment';
import { exportPostings } from '@/ui/reports/export-postings';
import { buttonCls } from '@/ui/style';
import { Table } from '@/ui/Table';
import { useAuth } from '@/lib/auth';
import { ArrowDown, ArrowUpDown } from 'lucide-react';
import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';

export const PersonPaymentsView = React.memo(function PersonPaymentsView({ id }: { id: string }) {
  const [query] = useQuery({
    query: PersonPaymentsDocument,
    variables: { id },
    pause: !id,
  });
  const person = query.data?.person;

  if (!person) {
    return null;
  }

  return (
    <div className="prose prose-accent mb-2">
      {person.unpaidPayments.length > 0 && <h3>K zaplacení</h3>}
      {person.unpaidPayments.map((x) => (
        <div key={x.id}>
          {x.payment?.cohortSubscription && (
            <h4>Členské příspěvky {x.payment.cohortSubscription.cohort?.name}</h4>
          )}
          <DefinitionList
            className="not-prose mb-2"
            items={{
              Částka: moneyFormatter.format(x.price),
              Účet: query.data?.tenant?.bankAccount || '-',
              'Variabilní symbol': x.payment?.variableSymbol,
              'Specifický symbol': x.payment?.specificSymbol,
              Zpráva: (
                <>
                  {person.firstName} {person.lastName},{' '}
                  {x.payment?.cohortSubscription?.cohort?.name ??
                    describePosting(x.payment, { personId: person.id })}
                </>
              ),
              Splatnost:
                x.payment?.dueAt && fullDateFormatter.format(new Date(x.payment.dueAt)),
            }}
          />

          {query.data?.tenant?.bankAccount && x.price?.amount && (
            <div className="border-4 border-transparent dark:border-white w-fit">
              <QRPayment
                acc={query.data.tenant.bankAccount}
                am={x.price.amount}
                cc={x.price.currency || 'CZK'}
                ss={x.payment?.specificSymbol}
                vs={x.payment?.variableSymbol}
                msg={`${person.firstName} ${person.lastName}, ${x.payment?.cohortSubscription?.cohort?.name ?? describePosting(x.payment, { personId: person.id })}`}
              />
            </div>
          )}
        </div>
      ))}

      {person.accountsList.length === 0 && <p>Žádné evidované platby</p>}
      {person.accountsList?.map((account) => (
        <div key={account.id}>
          <div className="flex flex-wrap justify-between">
            <div>
              Stav kreditu:{' '}
              {moneyFormatter.format({
                amount: account.balance,
                currency: account.currency,
              })}
            </div>

            <div className="flex gap-2">
              <button
                type="button"
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                onClick={() =>
                  exportPostings(
                    `${new Date().getFullYear()}-${new Date().getMonth()} ${person?.name}`,
                    account.postingsList || [],
                    person.id,
                  )
                }
              >
                Export XLSX
              </button>
            </div>
          </div>

          {account.postingsList.length > 0 && (
            <div>
              <h3>Minulé</h3>
              <AccountPaymentsTable account={account} />
            </div>
          )}
        </div>
      ))}
    </div>
  );
});

type Account = NonNullable<
  NonNullable<PersonPaymentsQuery['person']>['accountsList']
>[number];

function AccountPaymentsTable({ account }: { account: Account }) {
  const auth = useAuth();
  const [sort, setSort] = React.useState({ column: 'effectiveDate', descending: true });
  const baseRows = account.postingsList
    .filter(keyIsNonNull('transaction'))
    .map((row) => ({
      ...row,
      description:
        row.transaction.description ||
        describePosting(row.transaction.payment, { personId: account.personId }) ||
        '',
    }));
  const rows = baseRows.toSorted((a, b) => {
    const order =
      sort.column === 'amount'
        ? Number(a.amount) - Number(b.amount)
        : sort.column === 'description'
          ? compareText(a.description, b.description)
          : a.transaction.effectiveDate.localeCompare(b.transaction.effectiveDate);
    return sort.descending ? -order : order;
  });
  const paymentActionMap = useActionMap(
    paymentActions,
    baseRows.map((row) => row.transaction.payment).filter(isTruthy),
  );
  const transactionActionMap = useActionMap(
    transactionActions,
    baseRows.filter((row) => !row.transaction.payment).map((row) => row.transaction),
  );

  return (
    <Table
      className="not-prose"
      columns={[
        auth.isAdmin && (
          <span key="actions" className="sr-only">
            Akce
          </span>
        ),
        ...[
          { id: 'effectiveDate', label: 'Datum' },
          { id: 'description', label: 'Popis' },
          { id: 'amount', label: 'Částka' },
        ].map(({ id, label }) => (
          <div key={id} className={id === 'amount' ? 'text-right' : undefined}>
            <button
              type="button"
              aria-pressed={sort.column === id}
              className={buttonCls({
                display: 'none',
                variant: 'none',
                size: 'none',
                className:
                  'inline-flex items-center gap-1 whitespace-nowrap text-accent-11',
              })}
              onClick={() =>
                setSort((x) => ({
                  column: id,
                  descending: x.column === id ? !x.descending : false,
                }))
              }
            >
              {label}
              {sort.column === id ? (
                <>
                  <ArrowDown
                    className={cn('size-4', !sort.descending && 'rotate-180')}
                    aria-hidden
                  />
                  <span className="sr-only">
                    {sort.descending ? ', sestupně' : ', vzestupně'}
                  </span>
                </>
              ) : (
                <ArrowUpDown className="size-4" aria-hidden />
              )}
            </button>
          </div>
        )),
      ]}
    >
      {rows.map(({ id, amount, description, transaction }) => {
        const { payment } = transaction;
        const actions = payment
          ? paymentActionMap.get(payment.id)
          : transactionActionMap.get(transaction.id);

        return (
          <tr key={id} className="border-t border-neutral-4 align-top">
            {auth.isAdmin && (
              <td className="w-0">
                {actions && <ActionGroup variant="row" actions={actions} />}
              </td>
            )}
            <td className="whitespace-nowrap font-medium text-neutral-12">
              <time dateTime={transaction.effectiveDate}>
                {numericDateWithYearFormatter.format(new Date(transaction.effectiveDate))}
              </time>
            </td>
            <td className="text-neutral-12">
              {payment ? (
                <Link
                  href={`/platby/${payment.id}`}
                  className="text-accent-11 underline"
                >
                  {description || `Platba ${payment.id}`}
                </Link>
              ) : description}
            </td>
            <td className="whitespace-nowrap text-right font-semibold text-neutral-12">
              {moneyFormatter.format({ amount, currency: account.currency })}
            </td>
          </tr>
        );
      })}
    </Table>
  );
}
