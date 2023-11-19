import { useAuth } from './use-auth';
import React from 'react';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { buttonCls, buttonGroupCls, typographyCls } from './style';
import { AccountFragment, CreateCreditTransactionDocument } from '@/graphql/Payment';
import { useMutation } from 'urql';
import { TypeOf, z } from 'zod';
import { useZodForm } from '@/lib/use-schema-form';
import { useAsyncCallback } from 'react-async-hook';
import { NumberFieldElement } from './fields/number';
import { TextFieldElement } from './fields/text';
import { SubmitButton } from './submit';
import { moneyFormatter } from './format';

const Form = z.object({
  amount: z.number(),
  description: z.string().nullish().default(null),
});

export function CreateCreditTransactionForm({
  account,
  onSuccess,
}: {
  account: AccountFragment;
  onSuccess?: () => void;
}) {
  const { control, handleSubmit, watch } = useZodForm(Form, {
    defaultValues: {
      amount: 0,
      description: null,
    }
  });
  const [isDeposit, setIsDeposit] = React.useState(true);
  const create = useMutation(CreateCreditTransactionDocument)[1];

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await create({
      input: {
        vAccountId: account.id,
        vAmount: isDeposit ? values.amount : -values.amount,
        vDescription: values.description || (isDeposit ? 'Vklad kreditu' : 'Vyplacení kreditu'),
      },
    });
    onSuccess?.();
  });

  const balance = parseFloat(account.balance);
  const amount = watch('amount') || 0;
  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="description" label="Popis transakce" />

      <div className="flex flex-wrap gap-2 justify-between">
        <div className={buttonGroupCls({ className: 'self-center' })}>
          <button
            type="button"
            className={buttonCls({ variant: isDeposit ? 'primary' : 'outline' })}
            onClick={() => setIsDeposit(true)}
          >
            Vklad
          </button>
          <button
            type="button"
            className={buttonCls({ variant: !isDeposit ? 'primary' : 'outline' })}
            onClick={() => setIsDeposit(false)}
          >
            Výběr
          </button>
        </div>

        <div>
          <label className={typographyCls({ variant: 'label' })}>Částka</label>
          <NumberFieldElement
            control={control}
            name="amount"
            style={{ minWidth: '4rem' }}
            min={1}
            max={Number.MAX_SAFE_INTEGER}
            required
          />
        </div>

        <div className="text-right">
          <div>{moneyFormatter.format(balance)}</div>
          <div>{isDeposit ? '+' : '-'} {moneyFormatter.format(amount)}</div>
          <div className="border-t">{moneyFormatter.format(balance + (isDeposit ? +amount : -amount))}</div>
        </div>
      </div>

      <SubmitButton loading={onSubmit.loading} disabled={!amount} />
    </form>
  );
}

export function CreateCreditTransactionButton({ account }: { account: AccountFragment }) {
  const { perms } = useAuth();
  const [open, setOpen] = React.useState(false);

  if (!perms.isAdmin) {
    return null;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls()}>Ručně přidat/vyplatit kredit</button>
      </DialogTrigger>
      <DialogContent>
        <CreateCreditTransactionForm account={account} onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
}
