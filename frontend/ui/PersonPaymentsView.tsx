import { PersonPaymentsDocument, PersonPaymentsQuery } from '@/graphql/Person';
import React from 'react';
import {
  describePosting,
  fullDateFormatter,
  moneyFormatter,
  numericDateWithYearFormatter,
} from '@/ui/format';
import { useMutation, useQuery } from 'urql';
import { QRPayment } from '@/ui/QRPayment';
import { useTenant } from '@/ui/useTenant';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateCreditTransactionForm } from '@/ui/forms/CreateCreditTransactionForm';
import { exportPostings } from '@/ui/reports/export-postings';
import { buttonCls } from '@/ui/style';
import { useAuth } from './use-auth';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from './dropdown';
import { DeleteTransactionDocument } from '@/graphql/Payment';
import { PaymentMenu } from './EventView';
import { keyIsNonNull } from './truthyFilter';
import { Column, DataGrid, SortColumn } from 'react-data-grid';

export function PersonPaymentsView({ id }: { id: string }) {
  const auth = useAuth();
  const { data: tenant } = useTenant();
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
          <dl className="not-prose mb-2">
            <dt>Částka</dt>
            <dd>{moneyFormatter.format(x.price)}</dd>
            <dt>Účet</dt>
            <dd>1806875329/0800</dd>
            <dt>Variabilní symbol</dt>
            <dd>{x.payment?.variableSymbol}</dd>
            <dt>Specifický symbol</dt>
            <dd>{x.payment?.specificSymbol}</dd>
            <dt>Zpráva</dt>
            <dd>
              {person.firstName} {person.lastName},{' '}
              {x.payment?.cohortSubscription?.cohort?.name}
            </dd>
            {x.payment?.dueAt && (
              <>
                <dt>Splatnost</dt>
                <dd>{fullDateFormatter.format(new Date(x.payment?.dueAt))}</dd>
              </>
            )}
          </dl>

          {tenant?.bankAccount && x.price?.amount && (
            <div className="border-4 border-transparent dark:border-white w-fit">
              <QRPayment
                acc={tenant.bankAccount}
                am={x.price.amount}
                cc={x.price.currency || 'CZK'}
                ss={x.payment?.specificSymbol}
                vs={x.payment?.variableSymbol}
                msg={`${person.firstName} ${person.lastName}, ${x.payment?.cohortSubscription?.cohort?.name}`}
              />
            </div>
          )}
        </div>
      ))}

      {auth.isAdmin && (
        <div className="flex gap-2 my-2">
          <Dialog>
            <DialogTrigger size="sm" text="Ručně přidat/vyplatit kredit" />
            <DialogContent>
              <CreateCreditTransactionForm person={person} />
            </DialogContent>
          </Dialog>
        </div>
      )}
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
}

function TransactionMenu({ id, children }: { id: string; children?: React.ReactNode }) {
  const doDelete = useMutation(DeleteTransactionDocument)[1];
  const onDelete = React.useCallback(() => doDelete({ id }), [id, doDelete]);
  const auth = useAuth();
  if (!auth.isAdmin) return children;
  return (
    <DropdownMenu>
      <div className="flex gap-2">
        <DropdownMenuTrigger.RowDots />
        {children}
      </div>
      <DropdownMenuContent>
        <DropdownMenuButton onClick={onDelete}>Smazat platbu</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

type Account = NonNullable<
  NonNullable<PersonPaymentsQuery['person']>['accountsList']
>[number];
type Posting = NonNullable<Account['postingsList']>[number];
type PostingRow = Posting & { transaction: NonNullable<Posting['transaction']> };

function sortRows(rows: PostingRow[], sortColumns: readonly SortColumn[]) {
  if (sortColumns.length === 0) return rows;

  const { columnKey, direction } = sortColumns[0]!;
  const dir = direction === 'ASC' ? 1 : -1;

  const copy = [...rows];
  copy.sort((ra, rb) => {
    let cmp = 0;

    switch (columnKey) {
      case 'effectiveDate':
        cmp = ra.transaction.effectiveDate.localeCompare(rb.transaction.effectiveDate);
        break;
      case 'description':
        const da =
          ra.transaction.description ??
          describePosting(ra.transaction.payment ?? undefined, ra) ??
          '';
        const db =
          rb.transaction.description ??
          describePosting(rb.transaction.payment ?? undefined, rb) ??
          '';
        cmp = da.localeCompare(db);
        break;
      case 'amount':
        const aa = Number(ra.amount ?? 0);
        const bb = Number(rb.amount ?? 0);
        cmp = aa === bb ? 0 : aa < bb ? -1 : 1;
        break;
    }

    return cmp * dir;
  });

  return copy;
}

function AccountPaymentsTable({ account }: { account: Account }) {
  const auth = useAuth();
  const currency = account.currency ?? 'CZK';

  const baseRows = React.useMemo(() => {
    return (account.postingsList ?? []).filter(keyIsNonNull('transaction'));
  }, [account.postingsList]);

  const [sortColumns, setSortColumns] = React.useState<SortColumn[]>([
    { columnKey: 'effectiveDate', direction: 'DESC' },
  ]);

  const rows = React.useMemo(
    () => sortRows(baseRows, sortColumns),
    [baseRows, sortColumns],
  );

  const columns = React.useMemo(() => {
    const cols: Column<PostingRow>[] = [];

    if (auth.isAdmin) {
      cols.push({
        key: 'actions',
        name: '',
        width: 44,
        resizable: false,
        sortable: false,
        renderCell: ({ row }) => {
          const { transaction } = row;
          return (
            <div className="flex items-center justify-center">
              {transaction.payment ? (
                <PaymentMenu id={transaction.payment.id} />
              ) : (
                <TransactionMenu id={transaction.id} />
              )}
            </div>
          );
        },
      });
    }

    cols.push(
      {
        key: 'effectiveDate',
        name: 'Datum',
        sortable: true,
        renderCell: ({ row }) => (
          <time
            dateTime={row.transaction.effectiveDate}
            className="text-sm font-medium text-neutral-12"
          >
            {numericDateWithYearFormatter.format(new Date(row.transaction.effectiveDate))}
          </time>
        ),
      },
      {
        key: 'description',
        name: 'Popis',
        sortable: true,
        renderCell: ({ row }) => (
          <span className="text-sm text-neutral-12">
            {row.transaction.description ||
              describePosting(row.transaction.payment ?? undefined, row)}
          </span>
        ),
      },
      {
        key: 'amount',
        name: 'Částka',
        sortable: true,
        renderCell: ({ row }) => (
          <div className="text-right text-sm font-semibold text-neutral-12">
            {moneyFormatter.format({ amount: row.amount, currency })}
          </div>
        ),
        headerCellClass: 'justify-end',
        cellClass: 'justify-end',
      },
    );

    return cols;
  }, [auth.isAdmin, currency]);

  return (
    <div className="not-prose">
      <DataGrid
        columns={columns}
        rows={rows}
        rowKeyGetter={(r) => String(r.id)}
        sortColumns={sortColumns}
        onSortColumnsChange={setSortColumns}
        headerRowHeight={36}
        rowHeight={48}
        style={{ height: `${Math.min(rows.length * 6 + 10, 60)}vh` }}
      />
    </div>
  );
}
