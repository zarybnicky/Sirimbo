import { PersonPaymentsDocument } from "@/graphql/Person";
import React from "react";
import { describePosting, fullDateFormatter, moneyFormatter, numericDateFormatter } from "@/ui/format";
import { useMutation, useQuery } from "urql";
import { QRPayment } from "@/ui/QRPayment";
import { useTenant } from "@/ui/useTenant";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { CreateCreditTransactionForm } from "@/ui/forms/CreateCreditTransactionForm";
import { exportPostings } from "@/ui/reports/export-postings";
import { buttonCls } from "@/ui/style";
import { useAuth } from "./use-auth";
import { DropdownMenu, DropdownMenuButton } from "./dropdown";
import { DropdownMenuContent } from "@radix-ui/react-dropdown-menu";
import { DeleteTransactionDocument } from "@/graphql/Payment";
import { PaymentMenu } from "./EventView";

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
            <dd>{x.price?.amount} {x.price?.currency === 'CZK' ? 'Kč' : x.price?.currency}</dd>
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

          {tenant?.bankAccount && (
            <div className="border-4 border-transparent dark:border-white w-fit">
              <QRPayment
                acc={tenant.bankAccount}
                am={x.price?.amount}
                cc={x.price?.currency || 'CZK'}
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
            <div>Stav kreditu: {moneyFormatter.format(Number.parseFloat(account.balance))}</div>

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
            {account.postingsList.sort(
              (x, y) => (y.transaction?.effectiveDate || '').localeCompare(x.transaction?.effectiveDate || '')
            ).map(
              posting => posting.transaction ? (posting.transaction.payment ? (
                <div key={posting.id} className="justify-between gap-2 flex flex-wrap">
                  <PaymentMenu id={posting.transaction.payment.id}>
                    <span>
                      {numericDateFormatter.format(new Date(posting.transaction.effectiveDate))}{' '}
                      {posting.transaction.description || describePosting(posting.transaction.payment, posting)}
                    </span>
                    <span>{moneyFormatter.format(Number.parseFloat(posting.amount))}</span>
                  </PaymentMenu>
                </div>
              ) : (
                <div key={posting.id} className="justify-between gap-2 flex flex-wrap">
                  <TransactionMenu id={posting.transaction.id}>
                    <span>
                      {numericDateFormatter.format(new Date(posting.transaction.effectiveDate))}{' '}
                      {posting.transaction.description || describePosting(posting.transaction.payment, posting)}
                    </span>
                    <span>{moneyFormatter.format(Number.parseFloat(posting.amount))}</span>
                  </TransactionMenu>
                </div>
              )) : (
                <React.Fragment key={posting.id} />
              )
            )}
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
