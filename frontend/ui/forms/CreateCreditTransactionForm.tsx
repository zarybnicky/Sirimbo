import { CreateCreditTransactionDocument } from '@/graphql/Payment';
import { DatePickerElement } from '@/ui/fields/date';
import { NumberFieldElement } from '@/ui/fields/number';
import { TextFieldElement } from '@/ui/fields/text';
import { useFormResult } from '@/ui/form';
import { moneyFormatter } from '@/ui/format';
import { buttonCls, buttonGroupCls, typographyCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  date: z.date(),
  amount: z.number(),
  description: z.string().nullish().prefault(null),
});

export function CreateCreditTransactionForm({ person }: {
  person: {
    id: string;
    accountsList: { balance: string | null; }[];
  };
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, watch } = useForm({
    defaultValues: {
      date: new Date(),
      amount: 0,
      description: null,
    },
    resolver: zodResolver(Form),
  });
  const [isDeposit, setIsDeposit] = React.useState(true);
  const create = useMutation(CreateCreditTransactionDocument)[1];

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await create({
      input: {
        vDate: values.date.toISOString(),
        vPersonId: person.id,
        vCurrency: 'CZK',
        vAmount: (isDeposit ? values.amount : -values.amount).toString(),
        vDescription: values.description || (isDeposit ? 'Vklad kreditu' : 'Vyplacení kreditu'),
      },
    });
    onSuccess();
  });

  const balance = Number.parseFloat(person.accountsList.find(Boolean)?.balance ?? '0');
  const amount = watch('amount') || 0;
  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <DatePickerElement label="Datum" control={control} name="date" className="min-w-16" />
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
            step={0.01}
            min={0.01}
            max={Number.MAX_SAFE_INTEGER}
            required
          />
        </div>

        <div className="text-right m-3">
          <div>
            {moneyFormatter.format({ amount: balance.toString(), currency: 'CZK' })}
          </div>
          <div>
            {isDeposit ? '+ ' : '- '}
            {moneyFormatter.format({ amount: amount.toString(), currency: 'CZK' })}
          </div>
          <div className="border-t">
            {moneyFormatter.format({ amount: (balance + (isDeposit ? +amount : -amount)).toString(), currency: 'CZK' })}
          </div>
        </div>
      </div>

      <SubmitButton loading={onSubmit.loading} disabled={!amount} />
    </form>
  );
}
