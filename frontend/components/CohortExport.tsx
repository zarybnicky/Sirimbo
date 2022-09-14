import * as React from 'react';
import { $, Selector } from 'lib/zeus';
import Excel from 'exceljs';
import { saveAs } from 'file-saver';
import { useTypedQuery } from 'lib/query';

const ExportQuery = Selector('Query')({
  cohortMembers: [
    { id: $('id', 'BigInt!') },
    {
      nodes: {
        uJmeno: true,
        uPrijmeni: true,
        uRodneCislo: true,
        uTelefon: true,
        uEmail: true,
      },
    },
  ],
});

export function CohortExport({ id, name }: { id: number; name: string; }) {
  const { data } = useTypedQuery(['cohortExport', id], ExportQuery, {}, { variables: { id } });

  const saveData = async () => {
    if (!data) {
      return;
    }
    const workbook = new Excel.Workbook();
    const worksheet = workbook.addWorksheet(name || "Sheet 1");

    worksheet.columns = [
      { header: 'Jméno', key: 'firstName' },
      { header: 'Přijmení', key: 'lastName' },
      { header: 'Rodné číslo', key: 'birthNumber' },
      { header: 'Telefon', key: 'phone' },
      { header: 'E-mail', key: 'email' },
    ];

    worksheet.getRow(1).font = { bold: true };
    worksheet.columns.forEach(column => {
      column.width = (column?.header?.length || 0) + 10;
      column.alignment = { horizontal: 'center' };
    });

    data.cohortMembers?.nodes.forEach(x => worksheet.addRow({
      firstName: x.uJmeno,
      lastName: x.uPrijmeni,
      birthNumber: x.uRodneCislo,
      phone: x.uTelefon,
      email: x.uEmail,
    }));

    const buf = await workbook.xlsx.writeBuffer();
    saveAs(new Blob([buf]), `${name || "export-skupiny"}.xlsx`);
  };

  return <button className="btn btn-primary" onClick={(e) => {
    e.preventDefault();
    saveData();
  }}>Export</button>;
}
