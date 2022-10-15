import * as React from 'react';
import Excel from 'exceljs';
import { saveAs } from 'file-saver';
import { useMemberListQuery } from 'lib/graphql';

export function CohortExport({ id, name }: { id: string; name: string; }) {
  const { data } = useMemberListQuery({ cohortId: id });

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

    data.members?.nodes.forEach(x => worksheet.addRow({
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
