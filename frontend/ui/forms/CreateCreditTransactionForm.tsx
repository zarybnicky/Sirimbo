import { PersonPaymentsDocument } from '@/graphql/Person';
import { CreateCreditTransactionDocument } from '@/graphql/Payment';
import { DatePickerElement } from '@/ui/fields/date';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { moneyFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  date: z.date(),
  amount: z.number().positive('Zadejte kladnou částku'),
  description: z.string().nullish().prefault(null),
});

export function CreateCreditTransactionForm({ personId }: { personId: string }) {
  const [{ data }] = useQuery({
    query: PersonPaymentsDocument,
    variables: { id: personId },
    pause: !personId,
  });
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    defaultValues: {
      date: new Date(),
      amount: 0,
      description: null,
    },
    resolver: zodResolver(Form),
  });
  const [isDeposit, setIsDeposit] = React.useState(true);
  const [result, create] = useMutation(CreateCreditTransactionDocument);
  const person = data?.person;
  const currency = 'CZK';

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!person) return;

    const result = await create({
      input: {
        vDate: values.date.toISOString(),
        vPersonId: person.id,
        vCurrency: currency,
        vAmount: (isDeposit ? values.amount : -values.amount).toString(),
        vDescription:
          values.description || (isDeposit ? 'Vklad kreditu' : 'Vyplacení kreditu'),
      },
    });
    if (!result.error) onSuccess();
  };
  const amount = useWatch({ control, name: 'amount' }) || 0;

  if (!person) return null;

  const balance = Number.parseFloat(
    person.accountsList.find((x) => x.currency === currency)?.balance ?? '0',
  );
  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <DatePickerElement
        label="Datum"
        control={control}
        name="date"
        className="min-w-16"
      />
      <TextFieldElement control={control} name="description" label="Popis transakce" />

      <div className="flex flex-wrap gap-2 justify-between">
        <RadioButtonGroup
          className="self-center"
          value={isDeposit ? 'deposit' : 'withdrawal'}
          options={[
            { id: 'deposit', label: 'Vklad' },
            { id: 'withdrawal', label: 'Výběr' },
          ]}
          onValueChange={(x) => setIsDeposit(x === 'deposit')}
        />

        <TextFieldElement
          control={control}
          name="amount"
          label="Částka"
          className="w-24"
          type="number"
          step={0.01}
          min={0.01}
          max={Number.MAX_SAFE_INTEGER}
          required
        />

        <div className="text-right m-3">
          <div>
            {moneyFormatter.format({ amount: balance, currency })}
          </div>
          <div>
            {isDeposit ? '+ ' : '- '}
            {moneyFormatter.format({ amount, currency })}
          </div>
          <div className="border-t">
            {moneyFormatter.format({
              amount: balance + (isDeposit ? +amount : -amount),
              currency,
            })}
          </div>
        </div>
      </div>

      <SubmitButton control={control} disabled={!amount} />
    </form>
  );
}
