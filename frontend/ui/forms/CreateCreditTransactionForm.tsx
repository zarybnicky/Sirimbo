import { CreateCreditTransactionDocument } from '@/graphql/Payment';
import { useZodForm } from '@/lib/use-schema-form';
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
import { type TypeOf, z } from 'zod';

const Form = z.object({
  date: z.date(),
  amount: z.number(),
  description: z.string().nullish().default(null),
});

export function CreateCreditTransactionForm({ person }: {
  person: {
    id: string;
    accountsList: { balance: string; }[];
  };
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, watch } = useZodForm(Form, {
    defaultValues: {
      date: new Date(),
      amount: 0,
      description: null,
    }
  });
  const [isDeposit, setIsDeposit] = React.useState(true);
  const create = useMutation(CreateCreditTransactionDocument)[1];

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await create({
      input: {
        vDate: values.date.toISOString(),
        vPersonId: person.id,
        vCurrency: 'CZK',
        vAmount: isDeposit ? values.amount : -values.amount,
        vDescription: values.description || (isDeposit ? 'Vklad kreditu' : 'Vyplacení kreditu'),
      },
    });
    onSuccess();
  });

  const balance = Number.parseFloat(person.accountsList.find(x => x)?.balance ?? '0');
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
          <div>{moneyFormatter.format(balance)}</div>
          <div>{isDeposit ? '+' : '-'} {moneyFormatter.format(amount)}</div>
          <div className="border-t">{moneyFormatter.format(balance + (isDeposit ? +amount : -amount))}</div>
        </div>
      </div>

      <SubmitButton loading={onSubmit.loading} disabled={!amount} />
    </form>
  );
}
