import { PersonPaymentsDocument } from "@/graphql/Person";
import React from "react";
import { describePosting, fullDateFormatter, moneyFormatter } from "@/ui/format";
import { useMutation, useQuery } from "urql";
import { QRPayment } from "@/ui/QRPayment";
import { useTenant } from "@/ui/useTenant";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { CreateCreditTransactionForm } from "@/ui/forms/CreateCreditTransactionForm";
import { exportPostings } from "@/ui/reports/export-postings";
import { buttonCls } from "@/ui/style";
import { useAuth } from "./use-auth";
import { DropdownMenu, DropdownMenuButton, DropdownMenuTrigger } from "./dropdown";
import { DropdownMenuContent } from "@radix-ui/react-dropdown-menu";
import { DeleteTransactionDocument } from "@/graphql/Payment";
import { PaymentMenu } from "./EventView";
import { PaymentTransactionRow } from "@/ui/payments/PaymentTransactionRow";

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
            <div className="mt-2 overflow-hidden divide-y rounded-md border">
              {[...account.postingsList]
                .sort((a, b) => (b?.transaction?.effectiveDate || '').localeCompare(a?.transaction?.effectiveDate || ''))
                .map((posting) => {
                  if (!posting?.transaction) {
                    return null;
                  }

                  const { transaction } = posting;
                  const payment = transaction.payment;
                  const effectiveDate = transaction.effectiveDate
                    ? new Date(transaction.effectiveDate)
                    : null;
                  const description = transaction.description || describePosting(payment, posting);
                  const amount = posting.amount ?? '0';

                  const row = (
                    <PaymentTransactionRow
                      date={effectiveDate}
                      primaryLabel={description}
                      amount={amount}
                      currency="CZK"
                      variableSymbol={payment?.variableSymbol}
                      specificSymbol={payment?.specificSymbol}
                      paymentId={payment?.id}
                      showDebugLink={auth.isAdmin}
                    />
                  );

                  if (payment) {
                    return (
                      <PaymentMenu key={posting.id} id={payment.id}>
                        <DropdownMenuTrigger asChild>{row}</DropdownMenuTrigger>
                      </PaymentMenu>
                    );
                  }

                  return (
                    <TransactionMenu key={posting.id} id={transaction.id}>
                      <DropdownMenuTrigger asChild>{row}</DropdownMenuTrigger>
                    </TransactionMenu>
                  );
                })}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

function TransactionMenu({ id, children }: { id: string; children: React.ReactNode }) {
  const doDelete = useMutation(DeleteTransactionDocument)[1];
  const onDelete = React.useCallback(() => doDelete({ id }), [id, doDelete]);
  return (
    <DropdownMenu>
      {children}
      <DropdownMenuContent>
        <DropdownMenuButton onClick={onDelete}>Smazat platbu</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
