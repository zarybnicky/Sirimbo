import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTitle } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@app/ui/dropdown';
import { TextFieldElement } from '@app/ui/fields/text';
import { buttonCls } from '@app/ui/style';
import { useCountries } from '@app/ui/use-countries';
import { useZodForm } from '@/lib/use-schema-form';
import { Plus } from 'lucide-react';
import React from 'react';
import { TypeOf, z } from 'zod';
import { CheckboxElement } from './fields/checkbox';
import { DatePickerElement } from './fields/date';
import { toast } from 'react-toastify';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './submit';
import { useRouter } from 'next/router';
import { useMutation, useQuery } from 'urql';
import { CreatePersonDocument, FullPersonListDocument } from '@app/graphql/Person';

const Form = z.object({
  prefixTitle: z.string().default(''),
  firstName: z.string(),
  lastName: z.string(),
  suffixTitle: z.string().default(''),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED']),
  birthDate: z.string().nullish(),
  cstsId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatné IDT')
    .nullish(),
  wdsfId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatný MIN')
    .nullish(),
  nationalIdNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().default(''),
  email: z.string().email().optional(),
  phone: z.string().optional(),
  personId: z.string().nullish().default(null),
  isMember: z.boolean().default(false),
  isTrainer: z.boolean().default(false),
  isAdmin: z.boolean().default(false),
  sendInvitation: z.boolean().default(false),
  joinDate: z.date(),
});

export function CreatePersonDialog() {
  const [open, setOpen] = React.useState<'existing' | 'new' | null>(null);
  const router = useRouter();
  const create = useMutation(CreatePersonDocument)[1];

  const [personQuery] = useQuery({ query: FullPersonListDocument, pause: open !== 'existing' });
  const personOptions = React.useMemo(() => personQuery.data?.people?.nodes?.map(x => ({
    id: x.id,
    label: x.name || '?',
  })), [personQuery]);

  const countries = useCountries();
  const { control, handleSubmit, setValue, reset, watch } = useZodForm(Form);

  const personId = watch('personId');
  React.useEffect(() => {
    const person = personQuery.data?.people?.nodes.find(x => x.id === personId);
    if (person) {
      setValue('prefixTitle', person.prefixTitle);
      setValue('suffixTitle', person.suffixTitle);
      setValue('firstName', person.firstName);
      setValue('lastName', person.lastName);
      setValue('birthDate', person.birthDate);
      setValue('nationalIdNumber', person.nationalIdNumber);
      setValue('cstsId', person.cstsId);
      setValue('wdsfId', person.wdsfId);
      setValue('nationality', person.nationality);
      setValue('gender', person.gender);
      setValue('phone', person.phone || undefined);
      setValue('email', person.email || undefined);
      setValue('sendInvitation', false);
    }
  }, [personId]);

  React.useEffect(() => {
    if (open) {
      reset();
      setValue('isMember', true);
      setValue('sendInvitation', true);
      setValue('nationality', "203");
      setValue('joinDate', new Date());
    }
  }, [open])

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    const { personId, isAdmin, isMember, isTrainer, joinDate, sendInvitation, ...p } = data;
    const res = await create({
      input: {
        personId,
        p,
        sendInvitation,
        isAdmin,
        isMember,
        isTrainer,
        joinDate: joinDate.toISOString(),
      },
    });
    const id = res.data!.createPerson?.p?.id;
    if (id) {
      toast.success('Přidáno.');
      setOpen(null);
      router.replace(`/clenove/${id}`);
    }
  });

  return (
    <Dialog open={!!open} onOpenChange={() => setOpen(null)}>
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
            <Plus />
            Přidat osobu
          </button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end">
          <DropdownMenuButton onClick={() => setOpen('new')}>Nová osoba</DropdownMenuButton>
          <DropdownMenuButton onClick={() => setOpen('existing')}>Z jiného klubu</DropdownMenuButton>
        </DropdownMenuContent>
      </DropdownMenu>

      <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
        <DialogTitle>Nový člen</DialogTitle>

        <form className="space-2" onSubmit={handleSubmit(onSubmit.execute)}>
          {open === 'existing' && (
            <ComboboxElement
              control={control}
              className="col-full"
              name="personId"
              label="Existující osoba"
              placeholder="vyberte osobu"
              options={personOptions}
            />
          )}
          <fieldset disabled={open === 'existing'} className="grid lg:grid-cols-2 gap-2">
            <TextFieldElement control={control} name="prefixTitle" label="Titul před jménem" />
            <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />
            <TextFieldElement control={control} name="firstName" label="Jméno" required autoFocus />
            <TextFieldElement control={control} name="lastName" label="Příjmení" required />

            <TextFieldElement
              type="date"
              control={control}
              label="Datum narození"
              name="birthDate"
            />
            <TextFieldElement
              control={control}
              name="nationalIdNumber"
              label="Rodné číslo"
              placeholder="1111119999"
            />

            <TextFieldElement
              control={control}
              name="cstsId"
              label="ČSTS IDT"
              placeholder="10000000"
            />
            <TextFieldElement
              control={control}
              name="wdsfId"
              label="WDSF MIN"
              placeholder="10000000"
            />

            <ComboboxElement
              control={control}
              label="Národnost"
              name="nationality"
              placeholder="vyberte národnost"
              options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
            />
            <RadioButtonGroupElement
              control={control}
              name="gender"
              label="Pohlaví"
              options={[
                { id: 'MAN', label: 'Muž' },
                { id: 'WOMAN', label: 'Žena' },
              ]}
            />

            <TextFieldElement control={control} name="phone" label="Telefon" type="tel" />
            <TextFieldElement control={control} name="email" label="E-mail" type="email" />
          </fieldset>

          <div className="grid lg:grid-cols-2 gap-2">
            <div>
              <CheckboxElement control={control} name="isMember" label="Člen klubu" />
              <CheckboxElement control={control} name="isTrainer" label="Trenér" />
              <CheckboxElement control={control} name="isAdmin" label="Správce" />
              <CheckboxElement control={control} name="sendInvitation" label="Poslat pozvánku na e-mail" />
            </div>

            <DatePickerElement control={control} name="joinDate" label="Datum vstupu do klubu" />
          </div>

          <div className="col-span-2">
            <SubmitButton className="w-full" loading={onSubmit.loading}>
              Vytvořit
            </SubmitButton>
          </div>
        </form>
      </DialogContent>
    </Dialog>
  );
};
