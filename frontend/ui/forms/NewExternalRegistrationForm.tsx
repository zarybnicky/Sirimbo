import { RegisterToEventExternalDocument } from '@/graphql/Event';
import { TextAreaElement } from '@/ui/fields/textarea';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { countries } from '@/lib/countries';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  firstName: z.string().min(1, 'Zadejte jméno'),
  lastName: z.string().min(1, 'Zadejte příjmení'),
  prefixTitle: z.string().prefault(''),
  suffixTitle: z.string().prefault(''),
  nationality: z.string().min(1, 'Vyberte národnost'),
  birthDate: z.string().nullish(),
  taxIdentificationNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  email: z.email({ error: 'Zadejte platný e-mail' }),
  phone: z.string().min(9).max(14),
  note: z.string().prefault(''),
});

export function NewExternalRegistrationForm({ instanceId }: { instanceId: string }) {
  const { onSuccess } = useFormResult();
  const [result, create] = useMutation(RegisterToEventExternalDocument);

  const { control, handleSubmit } = useForm({
    defaultValues: {
      nationality: '203',
    },
    resolver: zodResolver(Form),
  });

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await create({
      input: {
        eventExternalRegistration: {
          instanceId,
          ...values,
        },
      },
    });
    if (!result.error && result.data?.createEventExternalRegistration) {
      toast.success('Přihlášení na akci proběhlo úspěšně.');
      onSuccess();
    }
  };

  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <fieldset className="grid lg:grid-cols-2 gap-2">
        <FormError error={result.error} />

        {/*<TextFieldElement
          control={control}
          name="prefixTitle"
          label="Titul před jménem"
        />
        <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />*/}
        <TextFieldElement
          control={control}
          name="firstName"
          label="Jméno"
          required
          autoFocus
        />
        <TextFieldElement control={control} name="lastName" label="Příjmení" required />

        <TextFieldElement
          control={control}
          name="email"
          type="email"
          label="E-mail"
          autoComplete="email"
          required
        />
        <TextFieldElement
          control={control}
          name="phone"
          type="tel"
          label="Telefon"
          autoComplete="tel"
          required
        />

        <ComboboxElement
          control={control}
          label="Národnost"
          name="nationality"
          placeholder="vyberte národnost"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
        {
          /*watch('nationality') === '203' ? (
          <TextFieldElement
            control={control}
            name="taxIdentificationNumber"
            label="Rodné číslo"
            placeholder="1111119999"
          />
        ) : */ <DatePickerElement
            control={control}
            name="birthDate"
            label="Datum narození"
            valueMode="date"
          />
        }

        <div className="col-full">
          <TextAreaElement control={control} name="note" label="Poznámky" />
        </div>
      </fieldset>

      <div className="col-full pt-2">
        <SubmitButton control={control} />
      </div>
    </form>
  );
}
