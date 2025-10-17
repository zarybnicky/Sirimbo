import { PersonPaymentsDocument, PersonPaymentsQuery } from "@/graphql/Person";
import React, { useMemo } from "react";
import { describePosting, fullDateFormatter, moneyFormatter, numericDateFormatter } from "@/ui/format";
import { useMutation, useQuery } from "urql";
import { QRPayment } from "@/ui/QRPayment";
import { useTenant } from "@/ui/useTenant";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { CreateCreditTransactionForm } from "@/ui/forms/CreateCreditTransactionForm";
import { exportPostings } from "@/ui/reports/export-postings";
import { buttonCls } from "@/ui/style";
import { useAuth } from "./use-auth";
import { DropdownMenu, DropdownMenuContent, DropdownMenuTrigger, DropdownMenuButton } from "./dropdown";
import { DeleteTransactionDocument } from "@/graphql/Payment";
import { PaymentMenu } from "./EventView";
import { isTruthy, keyIsNonNull } from "./truthyFilter";
import { ColumnDef, createColumnHelper } from "@tanstack/react-table";
import { DataTable } from "./DataTable";

export function PersonPaymentsView({ id }: { id: string }) {
  const auth = useAuth();
  const { data: tenant } = useTenant();
  const [query] = useQuery({ query: PersonPaymentsDocument, variables: { id }, pause: !id });
  const person = query.data?.person;

  if (!person) {
    return null;
  }

  return (
    <div className="prose prose-accent mb-2">
      {person.unpaidPayments.length > 0 && <h3>K zaplacení</h3>}
      {person.unpaidPayments.map(x => (
        <div key={x.id}>
          {x.payment?.cohortSubscription && (
            <h4>Členské příspěvky {x.payment.cohortSubscription.cohort?.name}</h4>
          )}
          <dl className="not-prose mb-2">
            <dt>Částka</dt>
            <dd>
              {moneyFormatter.format(x.price)}
            </dd>
            <dt>Účet</dt>
            <dd>1806875329/0800</dd>
            <dt>Variabilní symbol</dt>
            <dd>{x.payment?.variableSymbol}</dd>
            <dt>Specifický symbol</dt>
            <dd>{x.payment?.specificSymbol}</dd>
            <dt>Zpráva</dt>
            <dd>{person.firstName} {person.lastName}, {x.payment?.cohortSubscription?.cohort?.name}</dd>
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
        <div className="flex gap-2">
          <Dialog>
            <DialogTrigger size="sm" text="Ručně přidat/vyplatit kredit" />
            <DialogContent>
              <CreateCreditTransactionForm person={person} />
            </DialogContent>
          </Dialog>
        </div>
      )}
      {person.accountsList.length === 0 && <p>Žádné evidované platby</p>}
      {person.accountsList?.map(account => (
        <div key={account.id}>
          <div className="flex flex-wrap justify-between">
            <div>Stav kreditu: {moneyFormatter.format({ amount: account.balance, currency: account.currency})}</div>

            <div className="flex gap-2">
              <button
                type="button"
                className={buttonCls()}
                onClick={() => exportPostings(`${new Date().getFullYear()}-${new Date().getMonth()} ${person?.name}`, account.postingsList || [])}
              >
                Export XLSX
              </button>
            </div>
          </div>

          <div>
            <h3>Minulé</h3>
            <AccountPaymentsTable account={account} />
          </div>
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

type Account = NonNullable<NonNullable<PersonPaymentsQuery["person"]>["accountsList"]>[number];
type Posting = NonNullable<Account["postingsList"]>[number];
type PostingRow = Posting & { transaction: NonNullable<Posting["transaction"]> };

const columnHelper = createColumnHelper<PostingRow>();
const columns = (isAdmin: boolean, currency: string): ColumnDef<PostingRow, any>[] => ([
  isAdmin && columnHelper.display({
    id: "actions",
    enableSorting: false,
    cell: ({ row }) => {
      const { transaction } = row.original;
      return transaction.payment ? (
        <PaymentMenu id={transaction.payment.id} />
      ) : (
        <TransactionMenu id={transaction.id} />
      );
    },
  }),
  columnHelper.accessor('transaction.effectiveDate', {
    header: "Datum",
    sortingFn: 'datetime',
    cell: (info) => (
      <time dateTime={info.getValue()} className="text-sm font-medium text-neutral-12">
        {numericDateFormatter.format(new Date(info.getValue()))}
      </time>
    ),
  }),
  columnHelper.accessor("transaction.description", {
    header: "Popis",
    cell: ({ getValue, row }) => (
      <span className="text-sm text-neutral-12">
        {getValue() || describePosting(row.original.transaction.payment ?? undefined, row.original)}
      </span>
    ),
  }),
  columnHelper.accessor("amount", {
    header: "Částka",
    sortingFn: 'alphanumeric',
    cell: (info) => (
      <div className="text-right text-sm font-semibold text-neutral-12">
        {moneyFormatter.format({ amount: info.getValue(), currency })}
      </div>
    ),
  }),
].filter(isTruthy));

function AccountPaymentsTable({ account }: { account: Account }) {
  const auth = useAuth();
  const currency = account.currency ?? "CZK";
  const columnDef = useMemo(() => columns(auth.isAdmin, currency), [currency, auth.isAdmin]);
  const rows = (account.postingsList ?? [])
    .filter(keyIsNonNull("transaction")) as PostingRow[];
  rows.sort((a, b) => b.transaction.effectiveDate.localeCompare(a.transaction.effectiveDate));

  if (rows.length === 0) {
    return <p>Žádné pohyby.</p>;
  }

  return (
    <DataTable
      data={rows}
      columns={columnDef}
      enableSelection={false}
      enablePagination={false}
      toolbar={() => null}
      estimatedRowHeight={48}
    />
  );
}
