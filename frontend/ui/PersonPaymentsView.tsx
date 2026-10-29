import { DefinitionList } from '@/ui/DefinitionList';
import { PersonPaymentsDocument, PersonPaymentsQuery } from '@/graphql/Person';
import type { PostingFragment } from '@/graphql/Payment';
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
import { buttonCls } from '@/ui/style';
import { Table } from '@/ui/Table';
import { useAuth } from '@/lib/auth';
import { ArrowDown, ArrowUpDown, Download } from 'lucide-react';
import React from 'react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { useAsyncCallback } from 'react-async-hook';
import { saveAs } from 'file-saver';
import { Spinner } from '@/ui/Spinner';

export const PersonPaymentsView = React.memo(function PersonPaymentsView({
  id,
}: {
  id: string;
}) {
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
          <div>
            Stav kreditu:{' '}
            {moneyFormatter.format({
              amount: account.balance,
              currency: account.currency,
            })}
          </div>

          {account.postingsList.length > 0 && (
            <AccountPaymentsTable account={account} />
          )}
        </div>
      ))}
    </div>
  );
});

type Account = NonNullable<
  NonNullable<PersonPaymentsQuery['person']>['accountsList']
>[number];
type PostingRow = PostingFragment & {
  description: string;
  transaction: NonNullable<PostingFragment['transaction']>;
};

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
  const exportCurrentState = useAsyncCallback(() => exportPostings(account, rows));

  return (
    <Table
      className="not-prose"
      actions={
        <button
          type="button"
          className={buttonCls({ display: 'compact', variant: 'outline', size: 'sm' })}
          disabled={rows.length === 0 || exportCurrentState.loading}
          onClick={() => exportCurrentState.execute()}
        >
          {exportCurrentState.loading ? <Spinner /> : <Download aria-hidden />}
          Export
        </button>
      }
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
                <Link href={`/platby/${payment.id}`} className="text-accent-11 underline">
                  {description}
                </Link>
              ) : (
                description
              )}
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

async function exportPostings(account: Account, rows: readonly PostingRow[]) {
  const { Workbook } = await import('exceljs');
  const workbook = new Workbook();
  const worksheet = workbook.addWorksheet('Historie účtu');
  worksheet.columns = [
    { header: 'Datum', key: 'date', width: 14, style: { numFmt: 'dd.mm.yyyy' } },
    { header: 'Popis', key: 'description', width: 50 },
    {
      header: 'Částka',
      key: 'amount',
      width: 20,
      style: { numFmt: `#,##0.00 "${account.currency}"` },
    },
  ];
  worksheet.getRow(1).font = { bold: true };
  worksheet.addRows(
    rows.map(({ transaction, description, amount }) => {
      const date = new Date(transaction.effectiveDate);
      return {
        date: new Date(Date.UTC(date.getFullYear(), date.getMonth(), date.getDate())),
        description,
        amount: Number(amount),
      };
    }),
  );
  worksheet.addRow({});
  worksheet.addRow({
    description: 'Celkem',
    amount: {
      formula: `SUM(C2:C${rows.length + 1})`,
      result: rows.reduce((sum, row) => sum + Number(row.amount), 0),
    },
  });

  const buffer = await workbook.xlsx.writeBuffer();
  saveAs(
    new Blob([buffer]),
    `Historie účtu ${account.person?.name ?? account.id} (${account.currency}).xlsx`,
  );
}
