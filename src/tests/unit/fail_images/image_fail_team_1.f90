program image_fail_team_1
  use, intrinsic :: iso_fortran_env, only : team_type
  implicit none

  integer :: team_num
  type(team_type) :: odd_even_team, initial_team_minus_failed_images
   
  if (mod(num_images(), 2) == 1 .or. num_images() < 6) error stop "I need an even number of images (>= 6) to function"
  team_num = mod(this_image(), 2) + 1
  form team (team_num, odd_even_team)
 
  change team (odd_even_team)
    if (this_image() == num_images()) fail image
  end team

  !if (size(failed_images()) /= 2) error stop "failed_images()'s size should be two."
  form team (1, initial_team_minus_failed_images)
  
  change team (initial_team_minus_failed_images)
    write(*,*) this_image(), '/', num_images(), 'failed_images() == ', failed_images()
  end team
  
  write(*,*) 'after', this_image(), '/', num_images(), 'failed_images() == ', failed_images()
  write(*,*) this_image(), '/', num_images(), 'Test passed'

end program
