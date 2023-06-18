import { TextFieldElement } from '@app/ui/fields/text';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { toast } from 'react-toastify';
import { DateRange, DatePickerElement } from '@app/ui/fields/date';
import { RadioButtonGroup } from './RadioButtonGroup';
import { useForm } from 'react-hook-form';
import { Card } from './Card';
import { useMutation } from 'urql';
import { SubmitFormDocument } from '@app/graphql/Crm';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  date: z.date(),
  time: z.string(),
  location: z.string(),
  event: z.string(),
  exhibition: z.string().optional(),
  phone: z.string(),
  email: z.string().email(),
  notes: z.string().optional(),
  op: z.boolean().refine(x => x),
});
type FormProps = z.infer<typeof Form>;

export const ExhibitionRequestForm = () => {
  const submit = useMutation(SubmitFormDocument)[1];
  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });

  const onSubmit = useAsyncCallback(async ({ op, ...data }: FormProps) => {
    if (typeof fbq !== 'undefined') {
      fbq('track', 'SubmitApplication');
    }
    const url = window.location.toString();
    await submit({ type: 'Zájemce o vystoupení', data, url });
    toast.success('Brzy se vám ozveme!');
  });

  return (
    <Card>
      <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
        <h4 className="text-xl font-bold mb-2 col-full">
          Nezávazná poptávka tanečního vystoupení
        </h4>

        <div className="grid md:grid-cols-2 gap-2">
          <div>
            <DatePickerElement control={control} name="date" label="Termín" />
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
          </div>

          <div>
            <TextFieldElement
              control={control}
              name="time"
              label="Čas"
              type="time"
              required
            />
            <TextFieldElement control={control} name="location" label="Místo" required />
            <TextFieldElement control={control} name="event" label="Název akce" required />
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

            <TextAreaElement control={control} name="notes" label="Poznámky" />
          </div>
        </div>

        <CheckboxElement
          className="pt-4"
          control={control}
          name="op"
          value="agreed"
          required
          label={
            <>
              Souhlasím se{' '}
              <a
                className="text-accent-10"
                target="_blank"
                rel="noreferrer"
                href="/ochrana-osobnich-udaju"
              >
                zpracováním osobních údajů
              </a>
            </>
          }
        />

        <p className="py-2 prose prose-accent">Ozveme se Vám co nejdříve. V případě potřeby volejte 737 545 525.</p>

        <FormError error={onSubmit.error} />
        <SubmitButton loading={onSubmit.loading}>Odeslat</SubmitButton>
      </form>
    </Card>
  );
};
