import { ComboboxElement } from '@app/ui/Combobox';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { TextFieldElement } from '@app/ui/fields/text';
import { buttonCls } from '@app/ui/style';
import { useCountries } from '@app/ui/use-countries';
import { useZodForm } from 'lib/use-schema-form';
import { Plus } from 'lucide-react';
import React from 'react';
import { TypeOf, z } from 'zod';
import { CheckboxElement } from './fields/checkbox';
import { DatePickerElement } from './fields/date';
import { toast } from 'react-toastify';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './submit';
import { useRouter } from 'next/router';
import { useMutation } from 'urql';
import { CreatePersonDocument } from '@app/graphql/Person';

const Form = z.object({
  firstName: z.string(),
  lastName: z.string(),
  gender: z.enum(['MAN', 'WOMAN']),
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
  email: z.string().email().optional(),
  phone: z.string().optional(),
  isMember: z.boolean().default(true),
  isTrainer: z.boolean(),
  isAdmin: z.boolean(),
  sendInvitation: z.boolean(),
  joinDate: z.date(),
});

export function CreatePersonDialog() {
  const [open, setOpen] = React.useState(false);
  const router = useRouter();
  const create = useMutation(CreatePersonDocument)[1];

  const countries = useCountries();
  const { control, handleSubmit, setValue, reset } = useZodForm(Form);

  React.useEffect(() => {
    if (open) {
      reset();
      setValue('isMember', true);
      setValue('nationality', "203");
      setValue('joinDate', new Date());
    }
  }, [open])

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    const { isAdmin, isMember, isTrainer, joinDate, sendInvitation, ...p } = data;
    const res = await create({
      input: {
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
      setOpen(false);
      router.replace(`/clenove/${id}`);
    }
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
          <Plus />
          Vytvořit osobu
        </button>
      </DialogTrigger>
      <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
        <DialogTitle>Nový člen</DialogTitle>

        <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <TextFieldElement control={control} name="firstName" label="Jméno" required />
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
            options={[
              { id: 'MAN', label: 'Muž' },
              { id: 'WOMAN', label: 'Žena' },
            ]}
          />

          <TextFieldElement control={control} name="phone" label="Telefon" type="tel" />
          <TextFieldElement control={control} name="email" label="E-mail" type="email" />

          <div className="flex flex-col gap-2">
            <CheckboxElement control={control} name="isMember" label="Člen klubu" />
            <CheckboxElement control={control} name="isTrainer" label="Trenér" />
            <CheckboxElement control={control} name="isAdmin" label="Správce" />
            <CheckboxElement control={control} name="sendInvitation" label="Poslat pozvánku na e-mail" />
          </div>

          <DatePickerElement control={control} name="joinDate" label="Datum vstupu do klubu" />

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
