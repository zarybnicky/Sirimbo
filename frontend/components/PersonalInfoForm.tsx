import { useAuth } from 'lib/data/use-auth';
import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { RadioButtonGroupElement } from 'components/RadioButtomGroupElement';
import { TextFieldElement } from '@app/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { useCountries } from 'lib/data/use-countries';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { UpdateUserDocument } from '@app/graphql/User';
import { useMutation } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  uJmeno: z.string(),
  uPrijmeni: z.string(),
  uNarozeni: z.string(),
  uRodneCislo: z.string().regex(/[0-9]{9,10}/, 'Neplatné rodné číslo').optional(),
  uPohlavi: z.enum(['m', 'f']),
  uEmail: z.string().email(),
  uTelefon: z.string(),
  uStreet: z.string(),
  uConscriptionNumber: z.string().optional(),
  uOrientationNumber: z.string().optional(),
  uCity: z.string(),
  uDistrict: z.string().optional(),
  uPostalCode: z.string(),
  uNationality: z.string(),
  uPoznamky: z.string().optional(),
});
type FormProps = z.infer<typeof Form>;

export const PersonalInfoForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const { user } = useAuth();
  const countries = useCountries();
  const doUpdate = useMutation(UpdateUserDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    reset(Form.optional().parse(user));
  }, [reset, user]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ id: user?.id!, patch: values });
    onSuccess();
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <SubmitButton loading={onSubmit.loading} />

      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="uJmeno" label="Jméno" required />
      <TextFieldElement control={control} name="uPrijmeni" label="Příjmení" required />

      <TextFieldElement
        type="date"
        control={control}
        label="Datum narození"
        name="uNarozeni"
        required
      />
      <TextFieldElement
        control={control}
        name="uRodneCislo"
        label="Rodné číslo"
        required
        placeholder="1111119999"
      />

      <div>
        <RadioButtonGroupElement
          control={control}
          name="uPohlavi"
          options={[
            { id: 'm', label: 'Muž' },
            { id: 'f', label: 'Žena' },
          ]}
        />
      </div>

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Kontaktní údaje
      </div>

      <TextFieldElement
        control={control}
        type="email"
        name="uEmail"
        label="E-mail"
        required
      />
      <TextFieldElement
        control={control}
        type="tel"
        name="uTelefon"
        label="Telefon"
        required
      />

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Bydliště
      </div>

      <TextFieldElement control={control} name="uStreet" label="Ulice" required />

      <div className="grid grid-cols-2 gap-2">
        <TextFieldElement
          control={control}
          name="uConscriptionNumber"
          label="Č.popisné"
        />
        <TextFieldElement
          control={control}
          name="uOrientationNumber"
          label="Č.orientační"
        />
      </div>

      <TextFieldElement control={control} name="uCity" label="Město" required />
      <TextFieldElement control={control} name="uDistrict" label="Městská čtvrť" />
      <TextFieldElement control={control} name="uPostalCode" label="PSČ" />

      <div className="col-full">
        <ComboboxElement
          control={control}
          label="Národnost"
          name="uNationality"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
      </div>
    </form>
  );
};
