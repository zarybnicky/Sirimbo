import React from 'react';
import { Combobox, Transition } from '@headlessui/react'
import { useController, FieldError, FieldValues, Path, Control, ControllerProps } from 'react-hook-form';
import classNames from 'classnames';
import UnfoldMoreIcon from '@mui/icons-material/UnfoldMore';
import CheckIcon from '@mui/icons-material/Check';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

type Item = { id: string; label: string };
export type SelectElementProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  options: Item[];
  required?: boolean;
} & Extras;

export function SelectElement<TFieldValues extends FieldValues>({
  name, control, validation = {}, required, options, className, helperText, parseError, label,
}: SelectElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

  const { field: { value, onChange }, fieldState: { error } } = useController({
    name, control, rules: validation,
  });
  const [query, setQuery] = React.useState('')

  const filtered = query === '' ? options : options.filter(item =>
    item.label.toLowerCase().includes(query.toLowerCase()),
  );

  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;
  const valueObject = options.find(x => x.id === value);

  return (
    <Combobox value={valueObject} onChange={(x) => onChange(x.id)}>
      <div className={`relative mt-1 ${className}`}>
        <div className="text-slate-700 text-sm mb-1">{label}</div>
        <div className="relative w-full cursor-default overflow-hidden border-red-500 border focus-within:border-3 rounded-lg bg-white text-left shadow-sm focus:outline-none sm:text-sm">
          <Combobox.Input
            className="w-full border-none py-2 pl-3 pr-10 text-sm leading-5 text-gray-900 focus:ring-0"
            onChange={(event) => setQuery(event.target.value)}
            displayValue={(x?: Item) => x?.label || ''}
          />
          <Combobox.Button className="absolute inset-y-0 right-0 flex items-center pr-2">
            <UnfoldMoreIcon className="h-5 w-5 text-gray-400" aria-hidden="true" />
          </Combobox.Button>
        </div>
        <Transition
          as={React.Fragment}
          leave="transition ease-in duration-100"
          leaveFrom="opacity-100"
          leaveTo="opacity-0"
          afterLeave={() => setQuery('')}
        >
          <Combobox.Options className="z-10 absolute mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm">
            {/* {query.length > 0 && <Combobox.Option value={{ id: null, label: query }}>Create "{query}"</Combobox.Option>} */}
            {filtered.length === 0 && query !== '' ? (
              <div className="relative cursor-default select-none py-2 px-4 text-gray-700">
                Nic nenalezeno.
              </div>
            ) : (
              filtered.map((person) => (
                <Combobox.Option key={person.id} value={person} className={({ active }) =>
                  `relative cursor-default select-none py-2 pl-10 pr-4 ${active ? 'bg-red-600 text-white' : 'text-gray-900'}`
                }>
                  {({ selected, active }) => <>
                    <span className={`block truncate ${selected ? 'font-medium' : 'font-normal'}`}>
                      {person.label}
                    </span>
                    {selected && (
                      <span className={`absolute inset-y-0 left-0 flex items-center pl-3 ${active ? 'text-white' : 'text-red-600'}`}>
                        <CheckIcon className="h-5 w-5" aria-hidden="true" />
                      </span>
                    )}
                  </>}
                </Combobox.Option>
              ))
            )}
          </Combobox.Options>
        </Transition>
      </div>
      {parsedHelperText && (
        <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
      )}
    </Combobox>
  );
}