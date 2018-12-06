program image_fail_team_1
  use, intrinsic :: iso_fortran_env, only : team_type
  implicit none

  integer :: team_num
  type(team_type) :: team
   
! FIXME: doesn't work with ASSOCIATE?
! associate(np => num_images(), me => this_image())
    if (mod(np, 2) == 1 .or. np < 6) error stop "I need an even number of images (>= 6) to function"
    team_num = mod(me, 2) + 1
    form team (team_num, team)
 
    change team (team)
      if (this_image() == 1 .or. this_image() == num_images()) fail image
    end team

    !if (size(failed_images()) /= 2) error stop "failed_images()'s size should be two."
    write(*,*) me, '/', np, 'failed_images() == ', failed_images()
  
!   write(*,*) me, '/', np, 'Test passed'
 end associate

end program
