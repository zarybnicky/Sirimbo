import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { toast } from 'react-toastify';
import { DatePickerElement } from './DateRange';
import { RadioButtonGroup } from './RadioButtonGroup';
import { useForm } from 'react-hook-form';
import { Card } from './Card';
import { useMutation } from 'urql';
import { SubmitFormDocument } from 'lib/graphql/Crm';

export const ExhibitionRequestForm = () => {
  const submit = useMutation(SubmitFormDocument)[1];
  const { control, handleSubmit } = useForm();

  const onSubmit = useAsyncCallback(async ({ op, ...data }: any) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    const url = window.location.toString();
    await submit({ type: 'Zájemce o vystoupení', data, url });
    toast.success('Brzy se vám ozveme!');
  });

  return (
    <Card>
      <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="text-xl font-bold mb-2 col-full">
          Nezávazná poptávka tanečního vystoupení
        </h4>

        <div className="grid md:grid-cols-2 gap-2">
          <DatePickerElement control={control} name="date" label="Termín" required />
          <TextFieldElement
            control={control}
            name="time"
            label="Čas"
            type="time"
            required
          />
        </div>

        <TextFieldElement control={control} name="location" label="Místo" required />
        <TextFieldElement control={control} name="event" label="Název akce" required />

        <RadioButtonGroup
          control={control}
          label="Typ vystoupení"
          name="exhibition"
          options={[
            { value: 'Standardní tance', label: 'Standardní tance' },
            { value: 'Latinskoamerické tance', label: 'Latinskoamerické tance' },
            { value: 'Obě disciplíny', label: 'Obě disciplíny' },
            { value: 'Nevím', label: 'Nevím' },
            { value: 'Jiné', label: 'Jiné (doplňte v poznámce)' },
          ]}
        />

        <div className="grid sm:grid-cols-2 gap-2">
          <TextFieldElement
            control={control}
            name="phone"
            label="Telefon"
            type="tel"
            autoComplete="tel"
            required
          />

          <TextFieldElement
            control={control}
            name="email"
            label="E-mail"
            type="email"
            autoComplete="email"
            required
          />
        </div>

        <TextAreaElement control={control} name="notes" label="Poznámky" />

        <CheckboxElement
          className="my-2"
          control={control}
          name="op"
          value="agreed"
          required
          label={
            <>
              Souhlasím se{' '}
              <a
                className="text-red-500"
                target="_blank"
                rel="noreferrer"
                href="/ochrana-osobnich-udaju"
              >
                zpracováním osobních údajů
              </a>
            </>
          }
        />

        <p>Ozveme se Vám co nejdříve. V případě potřeby volejte 737 545 525.</p>

        <ErrorBox error={onSubmit.error} />
        <SubmitButton loading={onSubmit.loading}>Odeslat</SubmitButton>
      </form>
    </Card>
  );
};
