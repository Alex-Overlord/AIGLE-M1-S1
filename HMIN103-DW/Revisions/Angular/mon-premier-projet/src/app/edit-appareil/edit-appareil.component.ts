import { Router } from '@angular/router';
import { AppareilService } from './../services/appareil.service';
import { Component, OnInit } from '@angular/core';
import { NgForm } from "@angular/forms";

@Component({
  selector: 'app-edit-appareil',
  templateUrl: './edit-appareil.component.html',
  styleUrls: ['./edit-appareil.component.css']
})
export class EditAppareilComponent implements OnInit {

  defaultOnOff = 'éteint';

  constructor(private AppareilService: AppareilService, private router: Router) { }

  ngOnInit(): void {
  }
  
  onSubmit(form: NgForm) {
    const name = form.value['name'];
    const status = form.value['status'];
    this.AppareilService.addAppareil(name, status);
    this.router.navigate(['/appareils'])
  }

}
