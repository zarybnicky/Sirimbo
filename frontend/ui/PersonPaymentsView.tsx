import { PersonPaymentsDocument, type PersonPaymentsQuery } from "@/graphql/Person";
import React from "react";
import { ColumnDef } from "@tanstack/react-table";
import { DataTable } from "@/ui/DataTable";
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
import { keyIsNonNull } from "./truthyFilter";

export function PersonPaymentsView({ id }: { id: string }) {
  const auth = useAuth();
  const { data: tenant } = useTenant();
  const [query] = useQuery({ query: PersonPaymentsDocument, variables: { id }, pause: !id });
  const person = query.data?.person;

  if (!person) {
    return null;
  }

  const accounts = (person.accountsList ?? []).filter(Boolean) as Account[];

  return (
    <div className="space-y-8">
      {person.unpaidPayments.length > 0 && (
        <div className="prose prose-accent space-y-4">
          <h3>K zaplacení</h3>
          {person.unpaidPayments.map((x) => (
            <div key={x.id} className="space-y-3 rounded-xl border border-neutral-6 bg-neutral-1/80 p-4">
              {x.payment?.cohortSubscription && (
                <h4>Členské příspěvky {x.payment.cohortSubscription.cohort?.name}</h4>
              )}
              <dl className="not-prose grid grid-cols-1 gap-1 sm:grid-cols-2">
                <div>
                  <dt>Částka</dt>
                  <dd>{moneyFormatter.format(x.price)}</dd>
                </div>
                <div>
                  <dt>Účet</dt>
                  <dd>1806875329/0800</dd>
                </div>
                <div>
                  <dt>Variabilní symbol</dt>
                  <dd>{x.payment?.variableSymbol}</dd>
                </div>
                <div>
                  <dt>Specifický symbol</dt>
                  <dd>{x.payment?.specificSymbol}</dd>
                </div>
                <div className="sm:col-span-2">
                  <dt>Zpráva</dt>
                  <dd>
                    {person.firstName} {person.lastName}, {x.payment?.cohortSubscription?.cohort?.name}
                  </dd>
                </div>
                {x.payment?.dueAt && (
                  <div>
                    <dt>Splatnost</dt>
                    <dd>{fullDateFormatter.format(new Date(x.payment?.dueAt))}</dd>
                  </div>
                )}
              </dl>

              {tenant?.bankAccount && x.price?.amount && (
                <div className="w-fit border-4 border-transparent dark:border-white">
                  <QRPayment
                    acc={tenant.bankAccount}
                    am={x.price.amount}
                    cc={x.price.currency || "CZK"}
                    ss={x.payment?.specificSymbol}
                    vs={x.payment?.variableSymbol}
                    msg={`${person.firstName} ${person.lastName}, ${x.payment?.cohortSubscription?.cohort?.name}`}
                  />
                </div>
              )}
            </div>
          ))}
        </div>
      )}

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

      {accounts.length === 0 ? (
        <p className="text-sm text-neutral-11">Žádné evidované platby</p>
      ) : (
        accounts.map((account) => (
          <section key={account.id} className="space-y-4">
            <div className="flex flex-wrap items-center justify-between gap-3">
              <div className="text-sm text-neutral-12">
                Stav kreditu: {moneyFormatter.format({ amount: account.balance, currency: account.currency ?? "CZK" })}
              </div>
              <button
                type="button"
                className={buttonCls()}
                onClick={() =>
                  exportPostings(
                    `${new Date().getFullYear()}-${new Date().getMonth()} ${person?.name}`,
                    account.postingsList || []
                  )
                }
              >
                Export XLSX
              </button>
            </div>

            <div className="space-y-2">
              <h3 className="text-sm font-semibold text-neutral-11">Minulé</h3>
              <AccountPaymentsTable account={account} />
            </div>
          </section>
        ))
      )}
    </div>
  );
}

function TransactionMenu({ id, children }: { id: string; children: React.ReactNode }) {
  const doDelete = useMutation(DeleteTransactionDocument)[1];
  const onDelete = React.useCallback(() => doDelete({ id }), [id, doDelete]);
  const auth = useAuth();
  if (!auth.isAdmin) return children;
  return (
    <DropdownMenu>
      {children}
      <DropdownMenuContent>
        <DropdownMenuButton onClick={onDelete}>Smazat platbu</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

type Account = NonNullable<NonNullable<PersonPaymentsQuery["person"]>["accountsList"]>[number];
type Posting = NonNullable<Account["postingsList"]>[number];
type PostingRow = {
  id: string;
  posting: Posting;
  transaction: NonNullable<Posting["transaction"]>;
  payment: NonNullable<Posting["transaction"]>["payment"] | null;
};

function AccountPaymentsTable({ account }: { account: Account }) {
  const rows = React.useMemo<PostingRow[]>(() => {
    const list = (account.postingsList ?? []).filter(Boolean) as Posting[];
    const filtered = list.filter(keyIsNonNull("transaction"));
    return filtered
      .sort((a, b) => b.transaction.effectiveDate.localeCompare(a.transaction.effectiveDate))
      .map((posting) => {
        const transaction = posting.transaction;
        return {
          id: posting.id,
          posting,
          transaction,
          payment: transaction.payment,
        } satisfies PostingRow;
      });
  }, [account.postingsList]);

  const currency = account.currency ?? "CZK";

  const columns = React.useMemo<ColumnDef<PostingRow, unknown>[]>(
    () => [
      {
        id: "actions",
        header: "",
        size: 36,
        enableSorting: false,
        enableHiding: false,
        cell: ({ row }) => {
          const { payment, transaction } = row.original;
          const trigger = <DropdownMenuTrigger.RowDots className="text-neutral-10 hover:text-neutral-12" />;
          return payment ? (
            <PaymentMenu id={payment.id}>{trigger}</PaymentMenu>
          ) : (
            <TransactionMenu id={transaction.id}>{trigger}</TransactionMenu>
          );
        },
      },
      {
        id: "date",
        header: "Datum",
        accessorFn: (row) => row.transaction.effectiveDate,
        cell: ({ row }) => (
          <time dateTime={row.original.transaction.effectiveDate} className="text-sm font-medium text-neutral-12">
            {numericDateFormatter.format(new Date(row.original.transaction.effectiveDate))}
          </time>
        ),
      },
      {
        id: "description",
        header: "Popis",
        cell: ({ row }) => {
          const { posting, transaction, payment } = row.original;
          const description = transaction.description || describePosting(payment ?? undefined, posting);
          return <span className="text-sm text-neutral-12">{description}</span>;
        },
      },
      {
        id: "amount",
        header: "Částka",
        cell: ({ row }) => (
          <div className="text-right text-sm font-semibold text-neutral-12">
            {moneyFormatter.format({ amount: row.original.posting.amount, currency })}
          </div>
        ),
      },
    ],
    [currency]
  );

  if (rows.length === 0) {
    return <p className="text-sm text-neutral-11">Žádné pohyby.</p>;
  }

  return (
    <DataTable
      data={rows}
      columns={columns}
      enableSelection={false}
      enablePagination={false}
      toolbar={() => null}
      estimatedRowHeight={48}
    />
  );
}
